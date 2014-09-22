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
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait AsyncFunSpecLike extends FunSpecRegistration with AsyncTests { thisSuite =>

  import engine._

  val atMost = Duration(1000, MILLISECONDS)

  // TODO: Any better way?
  override protected def transformResult(testFun: => Future[Unit]): () => Outcome =
    () => {
      try {
        Await.result(testFun, atMost)
        Succeeded
      }
      catch {
        case ex: exceptions.TestCanceledException => Canceled(ex)
        case _: exceptions.TestPendingException => Pending
        case tfe: exceptions.TestFailedException => Failed(tfe)
        case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
      }
    }

  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf): Future[Outcome] = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      withAsyncFixture(
        new NoArgAsyncTest {
          val name = testData.name
          def apply(): Future[Outcome] = {
            Future { theTest.testFun() }
          }
          val configMap = testData.configMap
          val scopes = testData.scopes
          val text = testData.text
          val tags = testData.tags
        }
      )
    }

    runAsyncTestImpl(thisSuite, testName, args, true, invokeWithFixture, atMost)
  }

}