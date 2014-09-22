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
package org.scalatest.concurrent

import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest._
import exceptions.NotAllowedException

class AsyncFixturesSpec extends FunSpec {

  describe("AsyncFixtures") {

    it("should fail tests with NotAllowedException when mixed in classis style traits") {
      val spec = new FunSpec with AsyncFixtures {
        it("a test") {}
      }
      val rep = new EventRecordingReporter
      spec.run(None, Args(reporter = rep))
      assert(rep.testFailedEventsReceived.size == 1)
      val tfe = rep.testFailedEventsReceived(0)
      assert(tfe.throwable.isDefined)
      assert(tfe.throwable.get.isInstanceOf[NotAllowedException])
    }

    /*it("can be used to support concurrent tests") {

      import scala.concurrent._
      import org.scalatest.exceptions.{TestFailedException, TestFailedDueToTimeoutException}

      sealed trait ConcurrentTestResult
      case class AsyncErrorReporter(msg: String) extends ConcurrentTestResult
      case object Done extends ConcurrentTestResult

      trait ConcurrentTests extends SuiteMixin with AsyncFixtures { this: Suite with TestRegistration =>
        type Registration = Future[Outcome]
        implicit def defaultExecutionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
        implicit def convert(ctr: ConcurrentTestResult): Outcome = {
          ctr match {
            case AsyncErrorReporter(msg: String) => Failed(new TestFailedException(msg, 1))
            case Done => Succeeded
          }
        }
        implicit def convertToFuture(outcome: Outcome): Future[Outcome] = Future.successful(outcome)
      }

      trait ConcurrentFunSuite extends FunSuiteRegistration with ConcurrentTests {

      }

      class ExampleSpec extends ConcurrentFunSuite {
        override def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome] = {
          test() map {
            case Failed(ex: TestFailedDueToTimeoutException) => Canceled(ex)
            case outcome => outcome
          }
        }
        test("hi") {
          Future { Done }
        }
        test("ho") {
          Future { AsyncErrorReporter("off we go") }
        }
        test("ha") (Pending)
      }

      val spec = new ExampleSpec
      val rep = new EventRecordingReporter
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testSucceededEventsReceived.size == 1)

    }*/

  }

}