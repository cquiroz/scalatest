/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

import SharedHelpers.thisLineNumber

class AssertionsSpec extends FunSpec with OptionValues {
  
  val fileName: String = "AssertionsSpec.scala"

  describe("The === method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      assert(npe.getMessage === null)
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
      intercept[TestFailedException] {
        assert(a3 === a1)
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assert(n1 === n2)
      intercept[TestFailedException] {
        assert(n1 === "hi")
      }
      intercept[TestFailedException] {
        assert("hi" === n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        assert(n1 === a1)
      }
      intercept[TestFailedException] {
        assert(a1 === n1)
      }
    }
  }
  describe("The intercept method") {
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val caught =
          intercept[TestFailedException] {
            intercept[IllegalArgumentException] {
              throw wrongException
            }
          }
        assert(caught.cause.value eq wrongException)
      }
    }
    it("should catch subtypes of the given exception type") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      intercept[MyException] {
        throw new MyException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      intercept[MyException] {
        throw new MyExceptionSubClass
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod() {}
      }
      class AnotherException extends RuntimeException with MyTrait
      val caught = intercept[MyTrait] {
        throw new AnotherException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Make sure the result type is the type passed in, so I can 
      // not cast and still invoke any method on it I want
      caught.someRandomMethod()
    }

    it("should return the caught exception") {
      val e = new RuntimeException
      val result = intercept[RuntimeException] {
        throw e
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      assert(result eq e)
    }
  }
  
  describe("The newAssert method") {
    val a = 3
    val b = 5
    
    def wasNotEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasNotEqualTo", left, right)
      
    def wasEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasEqualTo", left, right)
      
    def expressionFailed(left: String): String = 
      FailureMessages("expressionFailed", UnquotedString(left))
      
    def wasNotGreaterThan(left: Any, right: Any): String = 
      FailureMessages("wasNotGreaterThan", left, right)
      
    def wasGreaterThan(left: Any, right: Any): String = 
      FailureMessages("wasGreaterThan", left, right)
      
    def wasNotGreaterThanOrEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasNotGreaterThanOrEqualTo", left, right)
      
    def wasGreaterThanOrEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasGreaterThanOrEqualTo", left, right)
      
    def wasNotLessThan(left: Any, right: Any): String = 
      FailureMessages("wasNotLessThan", left, right)
      
    def wasLessThan(left: Any, right: Any): String = 
      FailureMessages("wasLessThan", left, right)
      
    def wasNotLessThanOrEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasNotLessThanOrEqualTo", left, right)
      
    def wasLessThanOrEqualTo(left: Any, right: Any): String = 
      FailureMessages("wasLessThanOrEqualTo", left, right)
    
    it("should do nothing when is used to check a == 3") {
      newAssert(a == 3)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] { 
        newAssert(a == 5) 
      }
      assert(e.message === Some(wasNotEqualTo(UnquotedString("a"), 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check 5 == b") {
      newAssert(5 == b)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] { 
        newAssert(3 == b) 
      }
      assert(e.message === Some(wasNotEqualTo(3, UnquotedString("b"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check a != 5") {
      newAssert(a != 5)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] { 
        newAssert(a != 3) 
      }
      assert(e.message === Some(wasEqualTo(UnquotedString("a"), 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check 3 != b") {
      newAssert(3 != b)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] { 
        newAssert(5 != b) 
      }
      assert(e.message === Some(wasEqualTo(5, UnquotedString("b"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check 3 == 3") {
      newAssert(3 == 3)
    }
    
    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[TestFailedException] { 
        newAssert(3 == 5) 
      }
      assert(e.message === Some(expressionFailed("newAssert(3 == 5)")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] { 
        newAssert(a == b) 
      }
      assert(e.message === Some(wasNotEqualTo(UnquotedString("a"), UnquotedString("b"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestFailedException] { 
        newAssert(a == null) 
      }
      assert(e.message === Some(wasNotEqualTo(UnquotedString("a"), null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] { 
        newAssert(null == a) 
      }
      assert(e.message === Some(wasNotEqualTo(null, UnquotedString("a"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] { 
        newAssert(3 != a) 
      }
      assert(e.message === Some(wasEqualTo(3, UnquotedString("a"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check 5 != a") {
      newAssert(5 != a)
    }
    
    it("should do nothing when is used to check a > 2") {
      newAssert(a > 2)
    }
    
    it("should do nothing when is used to check 5 > a") {
      newAssert(5 > a)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] { 
        newAssert(a > 3) 
      }
      assert(e.message === Some(wasNotGreaterThan(UnquotedString("a"), 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] { 
        newAssert(3 > a) 
      }
      assert(e.message === Some(wasNotGreaterThan(3, UnquotedString("a"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check a >= 3") {
      newAssert(a >= 3)
    }
    
    it("should do nothing when is used to check 3 >= a") {
      newAssert(3 >= a)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] { 
        newAssert(a >= 4) 
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(UnquotedString("a"), 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] { 
        newAssert(2 >= a) 
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, UnquotedString("a"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check b < 6") {
      newAssert(b < 6)
    }
    
    it("should do nothing when is used to check 3 < b") {
      newAssert(3 < b)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] { 
        newAssert(b < 5) 
      }
      assert(e.message === Some(wasNotLessThan(UnquotedString("b"), 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] { 
        newAssert(5 < b) 
      }
      assert(e.message === Some(wasNotLessThan(5, UnquotedString("b"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check b <= 5") {
      newAssert(b <= 5)
    }
    
    it("should do nothing when is used to check 5 <= b") {
      newAssert(5 <= b)
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] { 
        newAssert(b <= 4) 
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(UnquotedString("b"), 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] { 
        newAssert(6 <= b) 
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(6, UnquotedString("b"))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
  }
}
