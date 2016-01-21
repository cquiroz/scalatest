/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest.concurrent.Timeouts._
import org.scalatest.exceptions.TimeoutField
import org.scalatest.time.Span
import org.scalatest._
import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalatest.exceptions.TestFailedDueToTimeoutException

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

trait AsyncTimeLimitedTests extends AsyncSuiteMixin { this: AsyncSuite =>

  abstract override def withFixture(test: NoArgAsyncTest): Future[Outcome] = {

    class TimeoutTask(promise: Promise[Outcome], span: Span) extends TimerTask {

      def run(): Unit = {
        if (!promise.isCompleted) {
          promise.complete(Success(Exceptional(new TestFailedDueToTimeoutException(sde => Some(Resources.testTimeLimitExceeded(span.prettyString)), None, getStackDepthFun("TimeLimiting.scala", "run"), None, span))))
        }
      }

    }

    try {

      val limit = timeLimit.totalNanos / 1000 / 1000
      val startTime = scala.compat.Platform.currentTime

      val future =
        failAfter(timeLimit) {
          super.withFixture(test)
        } (defaultTestInterruptor)

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

  /**
   * The time limit, in milliseconds, in which each test in a <code>Suite</code> that mixes in
   * <code>TimeLimitedTests</code> must complete.
   */
  def timeLimit: Span

  /**
   * The default <a href="Interruptor.html"><code>Interruptor</code></a> strategy used to interrupt tests that exceed their time limit.
   *
   * <p>
   * This trait's implementation of this method returns <a href="ThreadInterruptor$.html"><code>ThreadInterruptor</code></a>, which invokes <code>interrupt</code>
   * on the main test thread. Override this method to change the test interruption strategy.
   * </p>
   *
   * @return a <code>ThreadInterruptor</code>
   */
  val defaultTestInterruptor: Interruptor = ThreadInterruptor

}