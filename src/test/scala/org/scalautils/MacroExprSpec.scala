package org.scalautils

import org.scalatest._

class MacroExprSpec extends Spec {

  val me = this

  object `MacroExpr ` {

    class CustomInt(value: Int) {
      def :+(other: Int): CustomInt =
        new CustomInt(value + other)

      override def toString: String = value.toString
    }

    object `when work with member fields` {

      val a = 1
      val b = 2
      val aString = "a string"
      val aBoolean = true
      val aCustomInt = new CustomInt(8)
      val aList = List("one", "two", "three")

      def `should get MacroExpr for a.toChar correctly` {
        val expr = MacroExpr.expression(a.toChar)
        assert(expr.toString == "1.toChar")
      }

      def `should get MacroExpr for aString.startsWith("a") correctly` {
        val expr = MacroExpr.expression(aString.startsWith("a"))
        assert(expr.toString == "\"a string\".startsWith(\"a\")")
      }

      def `should get MacroExpr for aString.substring(2, 5) correctly` {
        val expr = MacroExpr.expression(aString.substring(2, 5))
        assert(expr.toString == "\"a string\".substring(2, 5)")
      }

      def `should get MacroExpr for aString.substring(2, 5).endsWith("r") correctly` {
        val expr = MacroExpr.expression(aString.substring(2, 5).endsWith("r"))
        assert(expr.toString == "\"a string\".substring(2, 5).endsWith(\"r\")")
      }

      def `should use symbolic notation for a == b` {
        val expr = MacroExpr.expression(a == b)
        assert(expr.toString == "1 == 2")
      }

      def `should use symbolic notation for a * b` {
        val expr = MacroExpr.expression(a * b)
        assert(expr.toString == "1 * 2")
      }

      def `should use symbolic notation for a / b` {
        val expr = MacroExpr.expression(a / b)
        assert(expr.toString == "1 / 2")
      }

      def `should use symbolic notation for a % b` {
        val expr = MacroExpr.expression(a % b)
        assert(expr.toString == "1 % 2")
      }

      def `should use symbolic notation for a + b` {
        val expr = MacroExpr.expression(a + b)
        assert(expr.toString == "1 + 2")
      }

      def `should use symbolic notation for a - b` {
        val expr = MacroExpr.expression(a - b)
        assert(expr.toString == "1 - 2")
      }

      def `should use symbolic notation for a < b` {
        val expr = MacroExpr.expression(a < b)
        assert(expr.toString == "1 < 2")
      }

      def `should use symbolic notation for a > b` {
        val expr = MacroExpr.expression(a > b)
        assert(expr.toString == "1 > 2")
      }

      def `should use symbolic notation for a <= b` {
        val expr = MacroExpr.expression(a <= b)
        assert(expr.toString == "1 <= 2")
      }

      def `should use symbolic notation for a >= b` {
        val expr = MacroExpr.expression(a >= b)
        assert(expr.toString == "1 >= 2")
      }

      def `should use symbolic notation for a ^ b` {
        val expr = MacroExpr.expression(a ^ b)
        assert(expr.toString == "1 ^ 2")
      }

      def `should use symbolic notation for a & b` {
        val expr = MacroExpr.expression(a & b)
        assert(expr.toString == "1 & 2")
      }

      def `should use symbolic notation for a | b` {
        val expr = MacroExpr.expression(a | b)
        assert(expr.toString == "1 | 2")
      }

      def `should use symbolic notation for !aBoolean` {
        val expr = MacroExpr.expression(!aBoolean)
        assert(expr.toString == "!true")
      }

      def `should use symbolic notation for +a` {
        val expr = MacroExpr.expression(+a)
        assert(expr.toString == "+1")
      }

      def `should use symbolic notation for -a` {
        val expr = MacroExpr.expression(-a)
        assert(expr.toString == "-1")
      }

      def `should use symbolic notation for ~a` {
        val expr = MacroExpr.expression(~a)
        assert(expr.toString == "~1")
      }

      def `should use symbolic notation for a != b` {
        val expr = MacroExpr.expression(a != b)
        assert(expr.toString == "1 != 2")
      }

      def `should use symbolic notation for aCustomInt :+ b` {
        val expr = MacroExpr.expression(aCustomInt :+ b)
        assert(expr.toString == "8 :+ 2")
      }

      def `should get MacroExpr for a + (a * 2) correctly` {
        val expr = MacroExpr.expression(a + (a * 2))
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for a + a * 2 correctly` {
        val expr = MacroExpr.expression(a + a * 2)
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for (a + a) * 2 correctly` {
        val expr = MacroExpr.expression((a + a) * 2)
        assert(expr.toString == "(1 + 1) * 2")
      }

      def `should get MacroExpr for 2 * (a + a) correctly ` {
        val expr = MacroExpr.expression(2 * (a + a))
        assert(expr.toString == "2 * (1 + 1)")
      }

      def `should get MacroExpr for aString.isInstanceOf[String] correct ` {
        val expr = MacroExpr.expression(aString.isInstanceOf[String])
        assert(expr.toString == "\"a string\".isInstanceOf[String]")
      }

      def `should get MacroExpr for "zero" :: aList correctly` {
        val expr = MacroExpr.expression("zero" :: aList)
        assert(expr.toString == "\"zero\" ::[String] List(\"one\", \"two\", \"three\")")
      }

      def `should get MacroExpr for aString :: aList correctly` {
        val expr = MacroExpr.expression(aString :: aList)
        assert(expr.toString == "\"a string\" ::[String] List(\"one\", \"two\", \"three\")")
      }

      /*def `should get MacroExpr for a === b correctly` {
        val expr = MacroExpr.expression(convertToEqualizer(a))
        assert(expr.toString == "1 === 2")
      }*/
    }

    object `when work with local variables in scope` {

      def `should get MacroExpr for a.toChar correctly` {
        val a = 1
        val expr = MacroExpr.expression(a.toChar)
        assert(expr.toString == "1.toChar")
      }

      def `should get MacroExpr for aString.startsWith("a") correctly` {
        val aString = "a string"
        val expr = MacroExpr.expression(aString.startsWith("a"))
        assert(expr.toString == "\"a string\".startsWith(\"a\")")
      }

      def `should get MacroExpr for aString.substring(2, 5) correctly` {
        val aString = "a string"
        val expr = MacroExpr.expression(aString.substring(2, 5))
        assert(expr.toString == "\"a string\".substring(2, 5)")
      }

      def `should get MacroExpr for aString.substring(2, 5).endsWith("r") correctly` {
        val aString = "a string"
        val expr = MacroExpr.expression(aString.substring(2, 5).endsWith("r"))
        assert(expr.toString == "\"a string\".substring(2, 5).endsWith(\"r\")")
      }

      def `should use symbolic notation for a == b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a == b)
        assert(expr.toString == "1 == 2")
      }

      def `should use symbolic notation for a * b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a * b)
        assert(expr.toString == "1 * 2")
      }

      def `should use symbolic notation for a / b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a / b)
        assert(expr.toString == "1 / 2")
      }

      def `should use symbolic notation for a % b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a % b)
        assert(expr.toString == "1 % 2")
      }

      def `should use symbolic notation for a + b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a + b)
        assert(expr.toString == "1 + 2")
      }

      def `should use symbolic notation for a - b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a - b)
        assert(expr.toString == "1 - 2")
      }

      def `should use symbolic notation for a < b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a < b)
        assert(expr.toString == "1 < 2")
      }

      def `should use symbolic notation for a > b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a > b)
        assert(expr.toString == "1 > 2")
      }

      def `should use symbolic notation for a <= b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a <= b)
        assert(expr.toString == "1 <= 2")
      }

      def `should use symbolic notation for a >= b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a >= b)
        assert(expr.toString == "1 >= 2")
      }

      def `should use symbolic notation for a ^ b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a ^ b)
        assert(expr.toString == "1 ^ 2")
      }

      def `should use symbolic notation for a & b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a & b)
        assert(expr.toString == "1 & 2")
      }

      def `should use symbolic notation for a | b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a | b)
        assert(expr.toString == "1 | 2")
      }

      def `should use symbolic notation for !aBoolean` {
        val aBoolean = true
        val expr = MacroExpr.expression(!aBoolean)
        assert(expr.toString == "!true")
      }

      def `should use symbolic notation for +a` {
        val a = 1
        val expr = MacroExpr.expression(+a)
        assert(expr.toString == "+1")
      }

      def `should use symbolic notation for -a` {
        val a = 1
        val expr = MacroExpr.expression(-a)
        assert(expr.toString == "-1")
      }

      def `should use symbolic notation for ~a` {
        val a = 1
        val expr = MacroExpr.expression(~a)
        assert(expr.toString == "~1")
      }

      def `should use symbolic notation for a != b` {
        val a = 1
        val b = 2
        val expr = MacroExpr.expression(a != b)
        assert(expr.toString == "1 != 2")
      }

      def `should use symbolic notation for aCustomInt :+ b` {
        val aCustomInt = new CustomInt(8)
        val b = 2
        val expr = MacroExpr.expression(aCustomInt :+ b)
        assert(expr.toString == "8 :+ 2")
      }

      def `should get MacroExpr for a + (a * 2) correctly` {
        val a = 1
        val expr = MacroExpr.expression(a + (a * 2))
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for a + a * 2 correctly` {
        val a = 1
        val expr = MacroExpr.expression(a + a * 2)
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for (a + a) * 2 correctly` {
        val a = 1
        val expr = MacroExpr.expression((a + a) * 2)
        assert(expr.toString == "(1 + 1) * 2")
      }

      def `should get MacroExpr for 2 * (a + a) correctly ` {
        val a = 1
        val expr = MacroExpr.expression(2 * (a + a))
        assert(expr.toString == "2 * (1 + 1)")
      }

      def `should get MacroExpr for aString.isInstanceOf[String] correct ` {
        val aString = "a string"
        val expr = MacroExpr.expression(aString.isInstanceOf[String])
        assert(expr.toString == "\"a string\".isInstanceOf[String]")
      }

      def `should get MacroExpr for "zero" :: aList correctly` {
        val aList = List("one", "two", "three")
        val expr = MacroExpr.expression("zero" :: aList)
        assert(expr.toString == "\"zero\" ::[String] List(\"one\", \"two\", \"three\")")
      }

      def `should get MacroExpr for aString :: aList correctly` {
        val aString = "a string"
        val aList = List("one", "two", "three")
        val expr = MacroExpr.expression(aString :: aList)
        assert(expr.toString == "\"a string\" ::[String] List(\"one\", \"two\", \"three\")")
      }
    }

    val a = 1
    val b = 2
    val aString = "a string"
    val aBoolean = true
    val aCustomInt = new CustomInt(8)
    val aList = List("one", "two", "three")

    object `when work with fields in outer scope` {

      def `should get MacroExpr for a.toChar correctly` {
        val expr = MacroExpr.expression(a.toChar)
        assert(expr.toString == "1.toChar")
      }

      def `should get MacroExpr for aString.startsWith("a") correctly` {
        val expr = MacroExpr.expression(aString.startsWith("a"))
        assert(expr.toString == "\"a string\".startsWith(\"a\")")
      }

      def `should get MacroExpr for aString.substring(2, 5) correctly` {
        val expr = MacroExpr.expression(aString.substring(2, 5))
        assert(expr.toString == "\"a string\".substring(2, 5)")
      }

      def `should get MacroExpr for aString.substring(2, 5).endsWith("r") correctly` {
        val expr = MacroExpr.expression(aString.substring(2, 5).endsWith("r"))
        assert(expr.toString == "\"a string\".substring(2, 5).endsWith(\"r\")")
      }

      def `should use symbolic notation for a == b` {
        val expr = MacroExpr.expression(a == b)
        assert(expr.toString == "1 == 2")
      }

      def `should use symbolic notation for a * b` {
        val expr = MacroExpr.expression(a * b)
        assert(expr.toString == "1 * 2")
      }

      def `should use symbolic notation for a / b` {
        val expr = MacroExpr.expression(a / b)
        assert(expr.toString == "1 / 2")
      }

      def `should use symbolic notation for a % b` {
        val expr = MacroExpr.expression(a % b)
        assert(expr.toString == "1 % 2")
      }

      def `should use symbolic notation for a + b` {
        val expr = MacroExpr.expression(a + b)
        assert(expr.toString == "1 + 2")
      }

      def `should use symbolic notation for a - b` {
        val expr = MacroExpr.expression(a - b)
        assert(expr.toString == "1 - 2")
      }

      def `should use symbolic notation for a < b` {
        val expr = MacroExpr.expression(a < b)
        assert(expr.toString == "1 < 2")
      }

      def `should use symbolic notation for a > b` {
        val expr = MacroExpr.expression(a > b)
        assert(expr.toString == "1 > 2")
      }

      def `should use symbolic notation for a <= b` {
        val expr = MacroExpr.expression(a <= b)
        assert(expr.toString == "1 <= 2")
      }

      def `should use symbolic notation for a >= b` {
        val expr = MacroExpr.expression(a >= b)
        assert(expr.toString == "1 >= 2")
      }

      def `should use symbolic notation for a ^ b` {
        val expr = MacroExpr.expression(a ^ b)
        assert(expr.toString == "1 ^ 2")
      }

      def `should use symbolic notation for a & b` {
        val expr = MacroExpr.expression(a & b)
        assert(expr.toString == "1 & 2")
      }

      def `should use symbolic notation for a | b` {
        val expr = MacroExpr.expression(a | b)
        assert(expr.toString == "1 | 2")
      }

      def `should use symbolic notation for !aBoolean` {
        val expr = MacroExpr.expression(!aBoolean)
        assert(expr.toString == "!true")
      }

      def `should use symbolic notation for +a` {
        val expr = MacroExpr.expression(+a)
        assert(expr.toString == "+1")
      }

      def `should use symbolic notation for -a` {
        val expr = MacroExpr.expression(-a)
        assert(expr.toString == "-1")
      }

      def `should use symbolic notation for ~a` {
        val expr = MacroExpr.expression(~a)
        assert(expr.toString == "~1")
      }

      def `should use symbolic notation for a != b` {
        val expr = MacroExpr.expression(a != b)
        assert(expr.toString == "1 != 2")
      }

      def `should use symbolic notation for aCustomInt :+ b` {
        val expr = MacroExpr.expression(aCustomInt :+ b)
        assert(expr.toString == "8 :+ 2")
      }

      def `should get MacroExpr for a + (a * 2) correctly` {
        val expr = MacroExpr.expression(a + (a * 2))
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for a + a * 2 correctly` {
        val expr = MacroExpr.expression(a + a * 2)
        assert(expr.toString == "1 + (1 * 2)")
      }

      def `should get MacroExpr for (a + a) * 2 correctly` {
        val expr = MacroExpr.expression((a + a) * 2)
        assert(expr.toString == "(1 + 1) * 2")
      }

      def `should get MacroExpr for 2 * (a + a) correctly ` {
        val expr = MacroExpr.expression(2 * (a + a))
        assert(expr.toString == "2 * (1 + 1)")
      }

      def `should get MacroExpr for aString.isInstanceOf[String] correct ` {
        val expr = MacroExpr.expression(aString.isInstanceOf[String])
        assert(expr.toString == "\"a string\".isInstanceOf[String]")
      }

      def `should get MacroExpr for "zero" :: aList correctly` {
        val expr = MacroExpr.expression("zero" :: aList)
        assert(expr.toString == "\"zero\" ::[String] List(\"one\", \"two\", \"three\")")
      }

      def `should get MacroExpr for aString :: aList correctly` {
        val expr = MacroExpr.expression(aString :: aList)
        assert(expr.toString == "\"a string\" ::[String] List(\"one\", \"two\", \"three\")")
      }
    }

  }

}
