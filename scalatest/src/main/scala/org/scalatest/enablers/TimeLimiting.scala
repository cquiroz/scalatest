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

import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.Span
import scala.concurrent.{Promise, Future, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.util.{Try, Failure, Success}
import java.util.{Timer, TimerTask}
import org.scalatest._

trait TimeLimiting[T] {

  def within(span: Span)(block: => T): T

}

object TimeLimiting {

  /*class TimeoutTask(promise: Promise[_], span: Span) extends TimerTask {

    def run(): Unit = {
      if (!promise.isCompleted) {
        promise.failure(new TestFailedDueToTimeoutException(sde => Some("test"), None, sde => 1, None, span))
      }
    }

  }

  implicit def timeLimitingNatureOfFuture[R]: TimeLimiting[Future[R]] =
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

  /*implicit def timeLimitingNatureOfStatus: TimeLimiting[Status] =
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
    }*/

  /*implicit def timeLimitingNatureOfAny: TimeLimiting[Any] =
    new TimeLimiting[Any] {

      def within(span: Span)(block: => Any): Any = {
        println("###the normal one??")
        val limit = span.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = block
          val endTime = scala.compat.Platform.currentTime
          val duration = endTime - startTime
          if (duration > limit)
            throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "within"), None, span)
          result
        }
        catch {
          case t: Throwable if !org.scalatest.Suite.anExceptionThatShouldCauseAnAbort(t) =>
            val endTime = scala.compat.Platform.currentTime
            val duration = endTime - startTime
            if (duration > limit)
              throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), Some(t), getStackDepthFun("TimeLimiting.scala", "within"), None, span)
            else
              throw t
        }
      }

    }

  implicit def timeLimitingNatureOfFutureOfAssertion: TimeLimiting[Future[org.scalatest.Assertion]] =
    new TimeLimiting[Future[Assertion]] {
      override def within(span: Span)(block: => Future[Assertion]): Future[Assertion] = {
        println("###go go")
        block
      }
    }*/

  implicit def timeLimitingNatureOfT[T]: TimeLimiting[T] =
    new TimeLimiting[T] {

      def within(span: Span)(block: => T): T = {
        val limit = span.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = block
          val endTime = scala.compat.Platform.currentTime
          val duration = endTime - startTime
          if (duration > limit)
            throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "within"), None, span)
          result
        }
        catch {
          case t: Throwable if !org.scalatest.Suite.anExceptionThatShouldCauseAnAbort(t) =>
            val endTime = scala.compat.Platform.currentTime
            val duration = endTime - startTime
            if (duration > limit)
              throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), Some(t), getStackDepthFun("TimeLimiting.scala", "within"), None, span)
            else
              throw t
        }
      }

    }

  implicit def timeLimitingNatureOfFuture[F <: Future[Assertion]]: TimeLimiting[F] =
    new TimeLimiting[F] {

      class TimeoutTask(promise: Promise[_], span: Span) extends TimerTask {

        def run(): Unit = {
          if (!promise.isCompleted) {
            promise.failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "run"), None, span))
          }
        }

      }

      def within(span: Span)(block: => F): F = {
        val limit = span.totalNanos / 1000 / 1000
        val startTime = scala.compat.Platform.currentTime
        try {
          val result = block
          val endTime = scala.compat.Platform.currentTime
          val duration = endTime - startTime
          if (duration > limit)
            throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "within"), None, span)
          else {
            val promise = Promise[Assertion]
            val task = new TimeoutTask(promise, span)
            val delay = limit - (scala.compat.Platform.currentTime - startTime)
            val timer = new Timer

            if (delay >= 0 && !result.isCompleted) {
              result.onComplete { t =>
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
              promise.future.asInstanceOf[F]  // Safe cast
            }
            else if (delay < 0) {
              promise.failure(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "within"), None, span))
              promise.future.asInstanceOf[F]  // Safe cast
            }
            else
              result
          }
        }
        catch {
          case t: Throwable if !org.scalatest.Suite.anExceptionThatShouldCauseAnAbort(t) =>
            val endTime = scala.compat.Platform.currentTime
            val duration = endTime - startTime
            if (duration > limit)
              throw new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), Some(t), getStackDepthFun("TimeLimiting.scala", "within"), None, span)
            else
              throw t
        }
      }
    }

}