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
package org.scalatest

import org.scalactic._
import exceptions.TestFailedException
import SharedHelpers.thisLineNumber

class DifferSpec extends FunSpec {

  import Matchers._

  describe("Differ") {

    describe("when used with default Equality") {

      it("should include differences in TestFailedException thrown from a shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual (3)
        }
        assert(e.message == Some("\"test2\" did not equal 3"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test2", 3)))
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          3 shouldEqual ("test2")
        }
        assert(e.message == Some("3 did not equal \"test2\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((3, "test2")))
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          3 shouldEqual (2)
        }
        assert(e.message == Some("3 did not equal 2"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((3, 2)))
      }

      it("should include differences in TestFailedException thrown from a should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" should equal (2)
        }
        assert(e.message == Some("\"test2\" did not equal 2"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test2", 2)))
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          2 should equal ("test2")
        }
        assert(e.message == Some("2 did not equal \"test2\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((2, "test2")))
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          2 should equal (3)
        }
        assert(e.message == Some("2 did not equal 3"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((2, 3)))
      }

      it("should include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test", 2)))
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((1, "test")))
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((1, 2)))
      }

      it("should include differences in TestFailedException thrown from all(a) should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test", 2)))
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((1, "test")))
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector((1, 2)))
      }

    }

    describe("when used with custom Equality that has custom difference implementation") {

      implicit val equality =
        new Equality[String] with Differ[String] {
          def areEqual(a: String, b: Any): Boolean = Equality.default.areEqual(a, b)

          def difference(a: String, b: Any): Difference =
            new Difference {
              def inlineDiff = {
                val (aa, bb) = Suite.getObjectsForFailureMessage(a, b)
                Some(("**" + aa, bb + "**"))
              }

              def sideBySideDiff = None

              def analysis = None
            }
        }

      it("can be used with a shouldEqual (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }

      it("can be used with a should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }

      it("can be used with all(a) shouldEqual (b) syntax") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }

      it("can be used with all(a) should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }
    }

  }

  val EOL = scala.compat.Platform.EOL

  sealed trait Parent
  case class Bar( s: String, i: Int ) extends Parent
  case class Foo( bar: Bar, b: List[Int], parent: Option[Parent] ) extends Parent

  describe("CaseClassDiffer") {

    it("should produce difference of 2 Bars correctly") {
      val a = Bar("asdf", 5)
      val b = Bar("asdf", 6)
      val c = Bar("asf", 6)

      assert(Differ.default.difference(a, b).analysis == Some("DifferSpec$Bar(i: 5 -> 6)"))
      assert(Differ.default.difference(b, c).analysis == Some("DifferSpec$Bar(s: as[d]f -> as[]f)"))
      assert(Differ.default.difference(a, c).analysis == Some("DifferSpec$Bar(i: 5 -> 6, s: as[d]f -> as[]f)"))
    }

    it("should produce difference of 2 Foos correctly") {
      val a: Foo = Foo(
        Bar( "asdf", 5 ),
        List( 123, 1234 ),
        Some( Bar( "asdf", 5 ) )
      )
      val b: Foo = Foo(
        Bar( "asdf", 66 ),
        List( 1234 ),
        Some( Bar( "qwer", 5 ) )
      )

      assert(Differ.default.difference(a, b).analysis == Some("DifferSpec$Foo(b: List(0: 123 -> 1234, 1: 1234 -> ), bar: DifferSpec$Bar(i: 5 -> 66), parent: Some(x: DifferSpec$Bar(s: [asdf] -> [qwer])))"))
    }

  }

  describe("GenSeqDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(Differ.default.difference(List(1, 2, 3), List(1, 2, 3)).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(Differ.default.difference(List(1, 2, 3), List(1, 6, 3)).analysis == Some("List(1: 2 -> 6)"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(Differ.default.difference(List(1, 2, 3), List(1, 2)).analysis == Some("List(2: 3 -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(Differ.default.difference(List(1, 2), List(1, 2, 3)).analysis == Some("List(2: -> 3)"))
    }

    it("should produce difference when elements in left and right is same but in different order") {
      assert(Differ.default.difference(List(1, 2, 3), List(3, 2, 1)).analysis == Some("List(0: 1 -> 3, 2: 3 -> 1)"))
    }

  }

  describe("GenSetDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(Differ.default.difference(Set(1, 2, 3), Set(1, 2, 3)).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(Differ.default.difference(Set(1, 2, 3), Set(1, 6, 3)).analysis == Some("Set(missingInLeft: [6], missingInRight: [2])"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(Differ.default.difference(Set(1, 2, 3), Set(1, 2)).analysis == Some("Set(missingInRight: [3])"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(Differ.default.difference(Set(1, 2), Set(1, 2, 3)).analysis == Some("Set(missingInLeft: [3])"))
    }

    it("should not produce difference when elements in left and right is same but in different order") {
      assert(Differ.default.difference(Set(1, 2, 3), Set(3, 2, 1)).analysis == None)
    }

  }

  describe("GenMapDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(Differ.default.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), List(1 -> "one", 2 -> "two", 3 -> "three")).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(Differ.default.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(1 -> "one", 6 -> "six", 3 -> "three")).analysis == Some("Map(2: two -> , 6: -> six)"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(Differ.default.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(1 -> "one", 2 -> "two")).analysis == Some("Map(3: three -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(Differ.default.difference(Map(1 -> "one", 2 -> "two"), Map(1 -> "one", 2 -> "two", 3 -> "three")).analysis == Some("Map(3: -> three)"))
    }

    it("should not produce difference when elements in left and right is same but in different order") {
      assert(Differ.default.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(3 -> "three", 2 -> "two", 1 -> "one")).analysis == None)
    }

  }

  describe("ProductDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(Differ.default.difference((1, 2, 3), (1, 2, 3)).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(Differ.default.difference((1, 2, 3), (1, 6, 3)).analysis == Some("Tuple3(_2: 2 -> 6)"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(Differ.default.difference((1, 2, 3), (1, 2)).analysis == Some("Tuple3(_3: 3 -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(Differ.default.difference((1, 2), (1, 2, 3)).analysis == Some("Tuple2(_3: -> 3)"))
    }

    it("should produce difference when elements in left and right is same but in different order") {
      assert(Differ.default.difference((1, 2, 3), (3, 2, 1)).analysis == Some("Tuple3(_1: 1 -> 3, _3: 3 -> 1)"))
    }

  }

}