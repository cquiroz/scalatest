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
package org.scalautils
import reflect.macros.Context

trait MacroExpr {
  val expression: Any
}

object MacroExpr {

  import scala.language.experimental.macros

  def applyExpr(expression: Any, qualifier: Any, name: String, decodedName: String, args: List[Any]): MacroExpr =
    ApplyMacroExpr(expression, qualifier, name, decodedName, args)

  def selectExpr(expression: Any, qualifier: Any, name: String, decodedName: String): MacroExpr =
    SelectMacroExpr(expression, qualifier, name, decodedName)

  def identExpr(expression: Any, name: String): MacroExpr =
    IdentMacroExpr(expression, name)

  def fallbackExpr(expression: Any, expressionText: String) =
    FallbackExpr(expression, expressionText)

  def expression(expr: Any): MacroExpr = macro MacroExpr.buildExpression

  def buildExpression(context: Context)(expr: context.Expr[Any]): context.Expr[MacroExpr] = {
    import context.universe._

    def valDef(name: String, rhs: Tree): ValDef =
      ValDef(
        Modifiers(),
        newTermName(name),
        TypeTree(),
        rhs
      )

    def fallback(tree: Tree): Tree =
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("org")),
              newTermName("scalautils")
            ),
            newTermName("MacroExpr")
          ),
          newTermName("fallbackExpr")
        ),
        List(
          tree,
          context.literal(show(tree)).tree
        )
      )

    def transformAst(tree: Tree): Tree =
       tree match {
        case apply: Apply =>
          apply.fun match {
            case select: Select =>
              val funValDef =
                valDef(
                  "$org_scalautils_macro_apply_qualifier",
                  select.qualifier.duplicate  // TODO: Probably need to traverse if it is an Apply
                )
              val argsValDef =
                apply.args.zipWithIndex.map { case (arg, idx) =>
                  valDef("$org_scalautils_macro_apply_arg_" + idx, arg)  // TODO: probably should traverse arg also
                }
              val argsNames = argsValDef.map(vd => Ident(vd.name))
              val newExpr =
                Apply(
                  Select(
                    Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                    select.name
                  ),
                  argsNames
                )

              val applyExprCall =
                Apply(
                  Select(
                    Select(
                      Select(
                        Ident(newTermName("org")),
                        newTermName("scalautils")
                      ),
                      newTermName("MacroExpr")
                    ),
                    newTermName("applyExpr")
                  ),
                  List(
                    newExpr,
                    Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                    context.literal(select.name.toString).tree,
                    context.literal(select.name.decoded).tree,
                    Apply(
                      Select(
                        Select(
                          Select(
                            Select(
                              Ident(newTermName("scala")),
                              newTermName("collection")
                            ),
                            newTermName("immutable")
                          ),
                          newTermName("List")
                        ),
                        newTermName("apply")
                      ),
                      argsNames
                    )
                  )
                )
              val codeInBlock: List[Tree] = List(funValDef) ++ argsValDef ++ List(applyExprCall)
              Block(codeInBlock: _*)

            case _ => fallback(tree) // TODO: Not sure what to do here
          }
        case select: Select =>
          val qualifierValDef =
            valDef(
              "$org_scalautils_macro_apply_qualifier",
              select.qualifier.duplicate  // TODO: Probably need to traverse if it is an Apply
            )
          val newExpr =
            Select(
              Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
              select.name
            )
          Block(
            qualifierValDef,
            Apply(
              Select(
                Select(
                  Select(
                    Ident(newTermName("org")),
                    newTermName("scalautils")
                  ),
                  newTermName("MacroExpr")
                ),
                newTermName("selectExpr")
              ),
              List(
                newExpr,
                Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                context.literal(select.name.toString).tree,
                context.literal(select.name.decoded).tree
              )
            )
          )

        case ident: Ident => ident.duplicate
        case thisTree: This => thisTree.duplicate
        case _ => fallback(tree)
      }

    val block = transformAst(expr.tree)
    context.Expr(block)
  }

}

private[scalautils] case class ApplyMacroExpr(expression: Any, qualifier: Any, name: String, decodedName: String, args: List[Any]) extends MacroExpr {

  private val symbolicSet =
    Set(
      "=="
    )

  private lazy val isSymbolic = symbolicSet.contains(decodedName) // TODO: better way to determine symbolic

  override def toString: String =
    if (isSymbolic)
      Prettifier.default(qualifier) + " " + decodedName + " " + args.map(Prettifier.default(_)).mkString(", ")
    else
      Prettifier.default(qualifier) + "." + decodedName + "(" + args.map(Prettifier.default(_)).mkString(", ") + ")"
}

private[scalautils] case class SelectMacroExpr(expression: Any, qualifier: Any, name: String, decodedName: String) extends MacroExpr {

  override def toString: String =
    Prettifier.default(qualifier) + "." + decodedName

}

private[scalautils] case class IdentMacroExpr(expression: Any, name: String) extends MacroExpr

private[scalautils] case class FallbackExpr(expression: Any, expressionText: String) extends MacroExpr