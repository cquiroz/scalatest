package org.scalautils

import org.scalatest._

class MacroExprSpec extends Spec {

  object `MacroExpr ` {

    val a = 1
    val b = 2
    val aString = "a string"

    def `should get MacroExpr for a == b correctly` {
      val expr = MacroExpr.expression(a == b)
      assert(expr.toString == "1 == 2")
    }

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
  }

}