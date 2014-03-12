package org.scalautils

import org.scalatest._

class MacroExprSpec extends Spec {

  object `MacroExpr ` {

    class CustomInt(value: Int) {
      def :+(other: Int): CustomInt =
        new CustomInt(value + other)

      override def toString: String = value.toString
    }

    val a = 1
    val b = 2
    val aString = "a string"
    val aBoolean = true
    val aCustomInt = new CustomInt(8)

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

  }

}