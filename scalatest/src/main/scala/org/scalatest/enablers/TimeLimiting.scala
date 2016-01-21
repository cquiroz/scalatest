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

import org.scalatest.concurrent.Interruptor
import org.scalatest.concurrent.Timeouts._
import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalatest.exceptions.{TimeoutField, TestFailedDueToTimeoutException}
import org.scalatest.time.Span
import scala.concurrent.{Promise, Future, ExecutionContext}
import scala.util.{Try, Failure, Success}
import java.util.{Timer, TimerTask}
import org.scalatest._

trait TimeLimiting[T] {

  def limitedTo(timeLimit: Span, interruptor: Interruptor)(block: => T): T

}

object TimeLimiting {

  implicit def timeLimitingNatureOfOutcome: TimeLimiting[Outcome] =
    new TimeLimiting[Outcome] {

      def limitedTo(timeLimit: Span, interruptor: Interruptor)(block: => Outcome): Outcome = {
        try {
          failAfter(timeLimit) {
            block
          } (interruptor)
        }
        catch {
          case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField =>
            Exceptional(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString))))
          case t: Throwable =>
            Exceptional(t)
        }
      }

    }

  implicit def timeLimitingNatureOfFutureOfOutcome(implicit executionContext: ExecutionContext): TimeLimiting[Future[Outcome]] =
    new TimeLimiting[Future[Outcome]] {

      class TimeoutTask(promise: Promise[Outcome], span: Span) extends TimerTask {

        def run(): Unit = {
          if (!promise.isCompleted) {
            promise.complete(Success(Exceptional(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "run"), None, span))))
          }
        }

      }

      def limitedTo(timeLimit: Span, interruptor: Interruptor)(block: => Future[Outcome]): Future[Outcome] = {

        try {
          val limit = timeLimit.totalNanos / 1000 / 1000
          val startTime = scala.compat.Platform.currentTime
          val future =
            failAfter(timeLimit) {
              block
            }(interruptor)

          val promise = Promise[Outcome]
          val task = new TimeoutTask(promise, timeLimit)
          val delay = limit - (scala.compat.Platform.currentTime - startTime)
          val timer = new Timer

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
          timer.schedule(task, delay)
          promise.future
        }
        catch {
          case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField =>
            Future.successful(Exceptional(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString)))))
          case t: Throwable =>
            Future.successful(Exceptional(t))
        }
      }

    }

}