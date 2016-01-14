/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.enablers

import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.Span
import scala.concurrent.{Promise, Future, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.util.{Try, Failure, Success}
import java.util.{Timer, TimerTask}
import org.scalatest.{Status, StatefulStatus, ScalaTestStatefulStatus}

trait TimeLimiting[T] {

  def within(span: Span)(block: => T): T

}

object TimeLimiting {

  class TimeoutTask(promise: Promise[_], span: Span) extends TimerTask {

    def run(): Unit = {
      if (!promise.isCompleted) {
        promise.failure(new TestFailedDueToTimeoutException(sde => Some("test"), None, sde => 1, None, span))
      }
    }

  }

  /*implicit def timeLimitingNatureOfFuture[R]: TimeLimiting[Future[R]] =
    new TimeLimiting[Future[R]] {
      def within(span: Span)(block: => Future[R]): Future[R] = {
        val promise = Promise[R]

        val timer = new Timer
        val task = new TimeoutTask(promise, span)
        timer.schedule(task, span.totalNanos / 1000 / 1000)

        val future: Future[R] = block
        future.onComplete { t =>
          t match {
            case Success(r) =>
              timer.cancel()
              if (!promise.isCompleted)
                promise.success(r)

            case Failure(e) =>
              timer.cancel()
              if (!promise.isCompleted)
                promise.failure(e)
          }
        }
        future
      }
    }*/

  implicit def timeLimitingNatureOfStatus: TimeLimiting[Status] =
    new TimeLimiting[Status] {

      class TimeoutTask(statefulStatus: ScalaTestStatefulStatus, span: Span) extends TimerTask {

        def run(): Unit = {
          if (!statefulStatus.isCompleted) {
            statefulStatus.setFailedWith(new TestFailedDueToTimeoutException(sde => Some("test"), None, sde => 1, None, span))
            statefulStatus.setCompleted()
          }
        }

      }

      def within(span: Span)(block: => Status): Status = {
        val statefulStatus = new ScalaTestStatefulStatus
        val task = new TimeoutTask(statefulStatus, span)

        val timer = new Timer

        val startTime = scala.compat.Platform.currentTime
        try {
          val status = block
          val limit = span.totalNanos / 1000 / 1000
          val delay = limit - (scala.compat.Platform.currentTime - startTime)

          if (delay >= 0 && !status.isCompleted) {
            status.whenCompleted { t =>
              if (!statefulStatus.isCompleted)
                t match {
                  case Success(r) =>
                    statefulStatus.setCompleted()

                  case Failure(e) =>
                    statefulStatus.setFailedWith(new TestFailedDueToTimeoutException(sde => Some("test"), Some(e), sde => 1, None, span))
                    statefulStatus.setCompleted()
                }
            }
            timer.schedule(task, delay)
            statefulStatus
          }
          else if (delay < 0) {
            statefulStatus.setFailedWith(new TestFailedDueToTimeoutException(sde => Some("test"), None, sde => 1, None, span))
            statefulStatus.setCompleted()
            statefulStatus
          }
          else
            status
        }
        catch {
          case t: Throwable =>
            statefulStatus.setFailedWith(new TestFailedDueToTimeoutException(sde => Some("test"), Some(t), sde => 1, None, span))
            statefulStatus.setCompleted()
            statefulStatus
        }
      }
    }

}