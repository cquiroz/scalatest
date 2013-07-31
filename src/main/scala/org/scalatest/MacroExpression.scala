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
package org.scalatest

sealed abstract class MacroExpression extends Function0[Boolean] {
  val value: Boolean
  val expressionText: String
  def apply: Boolean = value
  def &&(right: MacroExpression): MacroExpression
  def ||(right: MacroExpression): MacroExpression
  def unary_! : MacroExpression
  def okMessage: String
  def errorMessage: String
}

final class SimpleMacroExpression(expression: => Boolean, val expressionText: String) extends MacroExpression {
  
  def this(expression: MacroExpression, expressionText: String) = 
    this(expression.value, expressionText)
  
  lazy val value: Boolean = expression
  def &&(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "&&", right, value && right.value, "(" + expressionText + " && " + right.expressionText + ")")
  def ||(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "||", right, value || right.value, "(" + expressionText + " || " + right.expressionText + ")")
  def unary_! : MacroExpression = 
    new NotMacroExpression(this, "!", "!(" + expressionText + ")")
  def okMessage: String = FailureMessages("wasTrue", UnquotedString(expressionText))
  def errorMessage: String = FailureMessages("wasFalse", UnquotedString(expressionText))
}
  
final class NotMacroExpression(expression: MacroExpression, operator: String, val expressionText: String) extends MacroExpression { self => 
  
  lazy val value: Boolean = !expression.value
    
  def &&(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "&&", right, value && right.value, "(" + expressionText + " && " + right.expressionText + ")")
  def ||(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "||", right, value || right.value, "(" + expressionText + " || " + right.expressionText + ")")
  def unary_! : MacroExpression = 
    new NotMacroExpression(this, "!", "!(" + expressionText + ")")
  def okMessage: String = expression.errorMessage
  def errorMessage: String = expression.okMessage
}
  
final class BinaryMacroExpression(left: Any, operator: String, right: Any, expression: => Boolean, val expressionText: String) extends MacroExpression {
  
  def this(left: Any, operator: String, right: Any, expression: MacroExpression, expressionText: String) = 
    this(left, operator, right, expression.value, expressionText)
  
  lazy val value: Boolean = expression
    
  def &&(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "&&", right, value && right.value, "(" + expressionText + " && " + right.expressionText + ")")
    
  def ||(right: MacroExpression): MacroExpression = 
    new BinaryMacroExpression(this, "||", right, value || right.value, "(" + expressionText + " || " + right.expressionText + ")")
  
  def unary_! : MacroExpression = 
    new NotMacroExpression(this, "!", "!(" + expressionText + ")")
    
  def getText(obj: Any): String = 
    obj match {
      case expr: MacroExpression => expr.errorMessage
      case _ => "" + obj
    }
    
  def getObjectsForFailureMessage = 
    left match {
      case leftStr: String => {
        right match {
          case rightStr: String => {
            Suite.diffStrings(leftStr, rightStr)    
          }
          case _ => (left, right)
        }
      } 
      case _ => (left, right)
    }
  
  def okMessage: String = {
    operator match {
      case "==" => FailureMessages("equaled", left, right)
      case "===" => FailureMessages("equaled", left, right)
      case "!=" => FailureMessages("didNotEqual", left, right)
      case "!==" => FailureMessages("didNotEqual", left, right)
      case ">" => FailureMessages("wasGreaterThan", left, right)
      case ">=" => FailureMessages("wasGreaterThanOrEqualTo", left, right)
      case "<" => FailureMessages("wasLessThan", left, right)
      case "<=" => FailureMessages("wasLessThanOrEqualTo", left, right)
      case "&&" => 
        (left, right) match {
          case (leftExpr: MacroExpression, rightExpr: MacroExpression) => 
            FailureMessages("commaAnd", 
                            UnquotedString(if (leftExpr.value) leftExpr.okMessage else leftExpr.errorMessage), 
                            UnquotedString(if (rightExpr.value) rightExpr.okMessage else rightExpr.errorMessage))
          case (leftExpr: MacroExpression, rightAny: Any) => 
            FailureMessages("commaAnd", UnquotedString(if (leftExpr.value) leftExpr.okMessage else leftExpr.errorMessage), rightAny)
          case (leftAny: Any, rightExpr: MacroExpression) =>
            FailureMessages("commaAnd", leftAny, UnquotedString(if (rightExpr.value) rightExpr.okMessage else rightExpr.errorMessage))
          case _ =>
            FailureMessages("commaAnd", left, right)
        }
      case "||" => 
        (left, right) match {
          case (leftExpr: MacroExpression, rightExpr: MacroExpression) => 
            FailureMessages("commaAnd", 
                            UnquotedString(if (leftExpr.value) leftExpr.okMessage else leftExpr.errorMessage), 
                            UnquotedString(if (rightExpr.value) rightExpr.okMessage else rightExpr.errorMessage))
          case (leftExpr: MacroExpression, rightAny: Any) => 
            FailureMessages("commaAnd", UnquotedString(if (leftExpr.value) leftExpr.okMessage else leftExpr.errorMessage), rightAny)
          case (leftAny: Any, rightExpr: MacroExpression) =>
            FailureMessages("commaAnd", leftAny, UnquotedString(if (rightExpr.value) rightExpr.okMessage else rightExpr.errorMessage))
          case _ =>
            FailureMessages("commaAnd", left, right)
        }
      case _ => FailureMessages("wasTrue", UnquotedString(expressionText))
    }
  }
    
  def errorMessage: String = {
    operator match {
      case "==" => 
        val (leftee, rightee) = getObjectsForFailureMessage
        FailureMessages("didNotEqual", leftee, rightee)
      case "===" => 
        val (leftee, rightee) = getObjectsForFailureMessage
        FailureMessages("didNotEqual", leftee, rightee)
      case "!=" => FailureMessages("equaled", left, right)
      case "!==" => FailureMessages("equaled", left, right)
      case ">" => FailureMessages("wasNotGreaterThan", left, right)
      case ">=" => FailureMessages("wasNotGreaterThanOrEqualTo", left, right)
      case "<" => FailureMessages("wasNotLessThan", left, right)
      case "<=" => FailureMessages("wasNotLessThanOrEqualTo", left, right)
      case "&&" => 
        (left, right) match {
          case (leftExpr: MacroExpression, rightExpr: MacroExpression) => 
            if (leftExpr.value) 
              FailureMessages("commaBut", UnquotedString(leftExpr.okMessage), UnquotedString(rightExpr.errorMessage))
            else
              leftExpr.errorMessage
          case (leftExpr: MacroExpression, rightAny: Any) => 
            if (leftExpr.value) 
              FailureMessages("commaBut", UnquotedString(leftExpr.okMessage), rightAny)
            else
              leftExpr.errorMessage
          case (leftAny: Any, rightExpr: MacroExpression) =>
            FailureMessages("commaBut", leftAny, UnquotedString(if (rightExpr.value) rightExpr.okMessage else rightExpr.errorMessage))
          case _ =>
            FailureMessages("commaBut", left, right)
        }
      case "||" => 
        (left, right) match {
          case (leftExpr: MacroExpression, rightExpr: MacroExpression) => 
            FailureMessages("commaAnd", UnquotedString(leftExpr.errorMessage), UnquotedString(rightExpr.errorMessage))
          case (leftExpr: MacroExpression, rightAny: Any) => 
            FailureMessages("commaAnd", UnquotedString(leftExpr.errorMessage), rightAny)
          case (leftAny: Any, rightExpr: MacroExpression) =>
            FailureMessages("commaAnd", leftAny, UnquotedString(rightExpr.errorMessage))
          case _ =>
            FailureMessages("commaAnd", left, right)
        }
      case _ => FailureMessages("wasFalse", UnquotedString(expressionText))
    }
  }
}