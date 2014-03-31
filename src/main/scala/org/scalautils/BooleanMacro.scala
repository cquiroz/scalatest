/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalautils

import reflect.macros.Context

private[org] class BooleanMacro[C <: Context](val context: C, helperName: String) {

  /*
   * Translate the following:
   *
   * assert(something.aMethod == 3)
   *
   * to:
   *
   * {
   *   val $org_scalautils_assert_macro_left = something.aMethod
   *   val $org_scalautils_assert_macro_right = 3
   *   val $org_scalautils_assert_macro_result = $org_scalautils_assert_macro_left ==  $org_scalautils_assert_macro_result
   *   assertionsHelper.macroAssert($org_scalautils_assert_macro_left, "==", $org_scalautils_assert_macro_right, $org_scalautils_assert_macro_result, None)
   * }
   *
   */

  /*
   * Translate the following:
   *
   * assert(validate(1, 2, 3))
   *
   * to:
   *
   * assertionsHelper.macroAssert(validate(1, 2, 3), None)
   *
   */

  import context.universe._

  // Generate AST for:
  // val name = rhs
  def valDef(name: String, rhs: Tree): ValDef =
    ValDef(
      Modifiers(),
      newTermName(name),
      TypeTree(),
      rhs
    )

  def wrapInTempValAndFixup(name: String, tree: Tree): ValDef = {
    // Step 1: Explicitly create the symbol for the temporary val we're generating
    val powerContext = context.asInstanceOf[scala.reflect.macros.runtime.Context]
    val global = powerContext.global
    val enclosingOwner = powerContext.callsiteTyper.context.owner.asInstanceOf[Symbol]
    val tempValSymbol = enclosingOwner.newTermSymbol(newTermName(name))
    build.setTypeSignature(tempValSymbol, tree.tpe.widen)

    // Step 2: Fixup the tree in order to rewire symbol chains
    // These fixups are necessary to prevent bugs like https://github.com/scalatest/scalatest/issues/276
    // Read up on https://groups.google.com/forum/#!topic/scala-internals/rIyJ4yHdPDU for more information
    val tempValBody = tree.duplicate
    val oldOwner = enclosingOwner.asInstanceOf[global.Symbol]
    val newOwner = tempValSymbol.asInstanceOf[global.Symbol]
    object changeOwner extends global.ChangeOwnerTraverser(oldOwner, newOwner) {
      override def traverse(tree: global.Tree) {
        tree match {
          case _: global.DefTree => change(tree.symbol.moduleClass)
          case _ =>
        }
        super.traverse(tree)
      }
    }
    changeOwner.traverse(tempValBody.asInstanceOf[global.Tree])

    // Step 3: Reap the results of our hard work
    ValDef(tempValSymbol, tempValBody)
  }

  private val logicOperators = Set("&&", "||", "&", "|")

  private val supportedBinaryOperations =
    Set(
      "==",
      "!=",
      "===",
      "!==",
      "<",
      ">",
      ">=",
      "<=",
      "startsWith",
      "endsWith",
      "contains",
      "eq",
      "ne",
      "exists") ++ logicOperators

  private val supportedUnaryOperations =
    Set(
      "unary_!",
      "isEmpty"
    )

  private val lengthSizeOperations =
    Set(
      "length",
      "size"
    )

  def isSupportedBinaryOperator(operator: String) = supportedBinaryOperations.contains(operator)

  def isSupportedUnaryOperator(operator: String) = supportedUnaryOperations.contains(operator)

  def isSupportedLengthSizeOperator(operator: String) = lengthSizeOperations.contains(operator)

  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = {
    expr match {
      case literal: Literal =>
        getPosition(expr) match {
          case p: scala.reflect.internal.util.RangePosition => p.lineContent.slice(p.start, p.end).trim // this only available when -Yrangepos is enabled
          case p: reflect.internal.util.Position => ""
        }
      case _ => show(expr)
    }
  }

  def binaryMacroBool(select: Select, left: Symbol, right: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        Ident(right),
        Apply(
          Select(
            Ident(left),
            select.name
          ),
          List(Ident(right))
        )
      )
    )

  def typedBinaryMacroBool(select: Select, typeArgs: List[Tree], left: Symbol, right: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        Ident(right),
        Apply(
          TypeApply(
            Select(
              Ident(left),
              select.name
            ),
            typeArgs
          ),
          List(Ident(right))
        )
      )
    )

  def binaryMacroBool(select: Select, secondArg: Tree, left: Symbol, right: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        Ident(right),
        Apply(
          Apply(
            Select(
              Ident(left),
              select.name
            ),
            List(Ident(right))
          ),
          List(secondArg)
        )
      )
    )

  def simpleMacroBool(expression: Tree, expressionText: String): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("simpleMacroBool")
      ),
      List(
        expression,
        context.literal(expressionText).tree
      )
    )

  def notBool(left: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("notBool")
      ),
      List(
        Ident(left)
      )
    )

  def unaryMacroBool(select: Select, left: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("unaryMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        Select(
          Ident(left),
          select.name
        )
      )
    )

  def isInstanceOfMacroBool(select: Select, className: String, typeArg: Tree, left: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("isInstanceOfMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        context.literal(className).tree,
        TypeApply(
          Select(
            Ident(left),
            select.name
          ),
          List(typeArg)
        )
      )
    )

  def lengthSizeMacroBool(select: Select, left: Symbol, right: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("lengthSizeMacroBool")
      ),
      List(
        Ident(left),
        context.literal(select.name.decoded).tree,
        Select(
          Ident(left),
          select.name
        ),
        Ident(right)
      )
    )

  def existsMacroBool(select: Select, func: Function, left: Symbol, right: Symbol): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("existsMacroBool")
      ),
      List(
        Ident(left),
        Ident(right),
        Apply(
          Select(
            Ident(left),
            select.name
          ),
          List(
            func
          )
        )
      )
    )

  def traverseSelect(select: Select, rightExpr: Tree): (Tree, Tree) = {
    val operator = select.name.decoded
    if (logicOperators.contains(operator)) {
      val leftTree =
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate)
          case selectSelect: Select => transformAst(selectSelect.duplicate)
          case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier))
        }
      val rightTree = {
        val evalBlock =
          rightExpr match {
            case argApply: Apply => transformAst(argApply.duplicate)
            case argSelect: Select => transformAst(argSelect)
            case argTypeApply: TypeApply => transformAst(argTypeApply.duplicate)
            case _ => simpleMacroBool(rightExpr.duplicate, getText(rightExpr))
          }
        if (operator == "&&" || operator == "&")  {// generate if (left.value) {...} else false
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            evalBlock,
            simpleMacroBool(context.literal(false).tree, "")
          )
        }
        else if (operator == "||" || operator == "|") // || and |, generate if (left.value) true else {...}
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            simpleMacroBool(context.literal(true).tree, ""),
            evalBlock
          )
        else
          evalBlock
      }
      (leftTree, rightTree)
    }
    else
      (select.qualifier.duplicate, rightExpr.duplicate)
  }

  private def isPlaceHolder(tree: Tree): Boolean =
    tree match {
      case valDef: ValDef => valDef.rhs == EmptyTree
      case _ => false
    }

  def transformAst(tree: Tree): Tree = {
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if isSupportedBinaryOperator(select.name.decoded) =>
            val operator = select.name.decoded
            val (leftTree, rightTree) =  traverseSelect(select, apply.args(0))
            operator match {
              case "==" =>
                leftTree match {
                  case leftApply: Apply =>
                    leftApply.fun match {
                      case leftApplySelect: Select if isSupportedLengthSizeOperator(leftApplySelect.name.decoded) && leftApply.args.size == 0 => // support for a.length == xxx, a.size == xxxx
                        val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftApplySelect.qualifier.duplicate)
                        val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                        Block(
                          tempValLeft,
                          tempValRight,
                          lengthSizeMacroBool(leftApplySelect.duplicate, tempValLeft.symbol, tempValRight.symbol)
                        )
                      case _ =>
                        val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                        val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                        Block(
                          tempValLeft,
                          tempValRight,
                          binaryMacroBool(select.duplicate, tempValLeft.symbol, tempValRight.symbol)
                        )
                    }

                  case leftSelect: Select if isSupportedLengthSizeOperator(leftSelect.name.decoded) => // support for a.length == xxx, a.size == xxxx
                    val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftSelect.qualifier.duplicate)
                    val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                    Block(
                      tempValLeft,
                      tempValRight,
                      lengthSizeMacroBool(leftSelect.duplicate, tempValLeft.symbol, tempValRight.symbol)
                    )

                  case _ =>
                    val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                    val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                    Block(
                      tempValLeft,
                      tempValRight,
                      binaryMacroBool(select.duplicate, tempValLeft.symbol, tempValRight.symbol)
                    )
                }

              case "exists" =>
                rightTree match {
                  case func: Function if func.children.size == 2 && isPlaceHolder(func.children(0)) =>
                    val boolExpr = func.children(1)
                    boolExpr match {
                      case boolExprApply: Apply if boolExprApply.args.size == 1 =>
                        boolExprApply.fun match {
                          case boolExprApplySelect: Select if boolExprApplySelect.name.decoded == "==" =>
                            val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                            val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", boolExprApply.args(0).duplicate)
                            Block(
                              tempValLeft,
                              tempValRight,
                              existsMacroBool(select.duplicate, func.duplicate, tempValLeft.symbol, tempValRight.symbol)
                            )
                          case _ => simpleMacroBool(tree.duplicate, getText(tree))
                        }
                      case _ => simpleMacroBool(tree.duplicate, getText(tree))
                    }

                  case _ => simpleMacroBool(tree.duplicate, getText(tree))
                }

              case _ =>
                val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                Block(
                  tempValLeft,
                  tempValRight,
                  binaryMacroBool(select.duplicate, tempValLeft.symbol, tempValRight.symbol)
                )
            }

          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                Block(
                  tempValLeft,
                  tempValRight,
                  binaryMacroBool(select.duplicate, apply.args(0).duplicate, tempValLeft.symbol, tempValRight.symbol) // TODO: should apply.args(0) be traversed also?
                )
              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select if typeApply.args.size == 1 => // For TypeCheckedTripleEquals
                    val operator: String = select.name.decoded
                    if (operator == "===" || operator == "!==") {
                      val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                      val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                      val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                      Block(
                        tempValLeft,
                        tempValRight,
                        binaryMacroBool(select.duplicate, apply.args(0).duplicate, tempValLeft.symbol, tempValRight.symbol) // TODO: should apply.args(0) be traversed also?
                      )
                    }
                    else
                      simpleMacroBool(tree.duplicate, getText(tree))
                  case _ => simpleMacroBool(tree.duplicate, getText(tree))
                }
              case _ => simpleMacroBool(tree.duplicate, getText(tree))
            }

          case funTypeApply: TypeApply =>
            funTypeApply.fun match {
              case select: Select if isSupportedBinaryOperator(select.name.decoded) =>
                val (leftTree, rightTree) = traverseSelect(select, apply.args(0))
                val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
                val tempValRight = wrapInTempValAndFixup("$org_scalatest_assert_macro_right", rightTree)
                Block(
                  tempValLeft,
                  tempValRight,
                  typedBinaryMacroBool(select.duplicate, funTypeApply.args, tempValLeft.symbol, tempValRight.symbol)
                )

              case _ => simpleMacroBool(tree.duplicate, getText(tree))
            }

          case _ => simpleMacroBool(tree.duplicate, getText(tree))
        }
      case apply: Apply if apply.args.size == 0 => // for unary operation that takes 0 arguments
        apply.fun match {
          case select: Select if isSupportedUnaryOperator(select.name.decoded) =>
            val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", select.qualifier.duplicate)
            Block(
              tempValLeft,
              unaryMacroBool(select.duplicate, tempValLeft.symbol)
            )
          case _ => simpleMacroBool(tree.duplicate, getText(tree))
        }
      case typeApply: TypeApply if typeApply.args.length == 1 => // for isInstanceOf
        typeApply.fun match {
          case select: Select =>
            val operator = select.name.decoded
            if (operator == "isInstanceOf") {
              typeApply.args(0).tpe match {
                case typeRef: TypeRef =>
                  val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", select.qualifier.duplicate)
                  Block(
                    tempValLeft,
                    isInstanceOfMacroBool(select.duplicate, typeRef.sym.fullName, typeApply.args(0).duplicate, tempValLeft.symbol)
                  )
                case _ => simpleMacroBool(tree.duplicate, getText(tree))
              }
            }
            else
              simpleMacroBool(tree.duplicate, getText(tree))
          case _ => simpleMacroBool(tree.duplicate, getText(tree))
        }
      case select: Select if supportedUnaryOperations.contains(select.name.decoded) => // for ! and unary operation that does not take any arguments
          if (select.name.decoded == "unary_!") {
            val leftTree =
              select.qualifier match {
                case selectApply: Apply => transformAst(selectApply.duplicate)
                case selectSelect: Select => transformAst(selectSelect.duplicate)
                case selectTypeApply: TypeApply => transformAst(selectTypeApply.duplicate)
                case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier))
              }
            val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", leftTree)
            notBool(tempValLeft.symbol)
          }
          else {
            val tempValLeft = wrapInTempValAndFixup("$org_scalatest_assert_macro_left", select.qualifier.duplicate)
            Block(
              tempValLeft,
              unaryMacroBool(select.duplicate, tempValLeft.symbol)
            )
          }
      case _ =>
        simpleMacroBool(tree.duplicate, getText(tree))
    }
  }

  // Generate AST for:
  // helper.methodName(expression, clue)
  def callHelper(methodName: String, clueTree: Tree): Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName(methodName)
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")), clueTree)
    )

  def genMacro(booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any]): Expr[Unit] =
    context.Expr(
      Block(
        valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree)),
        callHelper(methodName, clueExpr.tree)
      )
    )
}