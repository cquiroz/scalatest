/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future

class AsyncFunSpecSpec extends FunSpec {

  describe("AsyncFunSpec") {

    sealed trait ConcurrentTestResult
    case class AsyncErrorReporter(msg: String) extends ConcurrentTestResult
    case object Done extends ConcurrentTestResult

    it("can be used with FunSpec to enable async testing") {

      class ExampleSpec extends AsyncFunSpecLike {

        it("test 1") {
          Future { Done }
        }

        it("test 2") {
          Future { Done }
        }

        it("test 3") {
          Future { Done }
        }
      }

      val spec = new ExampleSpec
      val rep = new EventRecordingReporter
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testSucceededEventsReceived.size == 3)
    }

    it("can be used to support different output type") {

      class RolandSpec extends AsyncFunSpecRegistration {
        type Registration = Future[ConcurrentTestResult]

        override protected def transformResult(testFun: => Future[ConcurrentTestResult]): () => Outcome =
          () => {
            println("###here!!!")
            try {
              val result = scala.concurrent.Await.result(testFun, atMost)
              result match {
                case AsyncErrorReporter(msg: String) => Failed(new TestFailedException(msg, 1))
                case Done => Succeeded
              }
            }
            catch {
              case ex: exceptions.TestCanceledException => Canceled(ex)
              case _: exceptions.TestPendingException => Pending
              case tfe: exceptions.TestFailedException => Failed(tfe)
              case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
            }
          }
      }

      class ExampleSpec extends RolandSpec {
        it("hi") {
          Future { Done }
        }
        it("ho") {
          Future { AsyncErrorReporter("off we go") }
        }
        //it("ha") (Pending)
      }

      val spec = new ExampleSpec
      val rep = new EventRecordingReporter
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testSucceededEventsReceived.size == 1)

    }

  }

}