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

trait MacroExpr[T] {
  def value: T
}

object MacroExpr {

  import scala.language.experimental.macros

  def applyExpr[T](value: T, qualifier: Any, name: String, decodedName: String, args: List[Any]): MacroExpr[T] =
    new ApplyMacroExpr(value, qualifier, name, decodedName, args)

  def typeApplyExpr[T](value: T, qualifier: Any, name: String, decodedName: String, types: List[String], args: List[Any]): MacroExpr[T] =
    TypeApplyMacroExpr(value, qualifier, name, decodedName, types, args)

  def selectExpr[T](value: T, qualifier: Any, name: String, decodedName: String): MacroExpr[T] =
    SelectMacroExpr(value, qualifier, name, decodedName)

  def identExpr[T](value: T, name: String, decodedName: String): MacroExpr[T] =
    IdentMacroExpr(value, name, decodedName)

  def thisExpr[T](value: T): MacroExpr[T] =
    ThisExpr(value)

  def fallbackExpr[T](value: T, expressionText: String): MacroExpr[T] =
    FallbackExpr(value, expressionText)

  def expression[T](expr: T): MacroExpr[T] = macro MacroExpr.buildExpression[T]

  def buildExpression[T](context: Context)(expr: context.Expr[T]): context.Expr[MacroExpr[T]] = {
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

    def list(elements: List[Tree]): Tree =
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
        elements
      )

    def macroExpr(methodName: String, args: List[Tree]): Tree =
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("org")),
              newTermName("scalautils")
            ),
            newTermName("MacroExpr")
          ),
          newTermName(methodName)
        ),
        args
      )

    def selectQualifierValue(select: Select): Select =
      Select(
        Select(
          Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
          newTermName("value")
        ),
        select.name
      )

    def transformAst(tree: Tree): Tree =
      tree match {
        case apply: Apply =>
          apply.fun match {
            case select: Select =>
              val funValDef = valDef("$org_scalautils_macro_apply_qualifier", transformAst(select.qualifier))
              val argsValDef =
                apply.args.zipWithIndex.map { case (arg, idx) =>
                  valDef("$org_scalautils_macro_apply_arg_" + idx, transformAst(arg))
                }
              val argsNames = argsValDef.map(vd => Ident(vd.name))
              val newExpr = Apply(selectQualifierValue(select), argsNames.map(arg => Select(arg, newTermName("value"))))

              val applyExprCall =
                macroExpr(
                  "applyExpr",
                  List(
                    newExpr,
                    Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                    context.literal(select.name.toString).tree,
                    context.literal(select.name.decoded).tree,
                    list(argsNames)
                  )
                )

              val codeInBlock: List[Tree] = List(funValDef) ++ argsValDef ++ List(applyExprCall)
              Block(codeInBlock: _*)

              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select =>
                    val funValDef = valDef("$org_scalautils_macro_apply_qualifier", transformAst(select.qualifier))
                    val types = typeApply.args.map(t => context.literal(show(t)).tree)
                    val argsValDef =
                      apply.args.zipWithIndex.map { case (arg, idx) =>
                        valDef("$org_scalautils_macro_apply_arg_" + idx, transformAst(arg))
                      }
                    val argsNames = argsValDef.map(vd => Ident(vd.name))

                    val newExpr =
                      Apply(
                        TypeApply(selectQualifierValue(select), typeApply.args),
                        argsNames.map(arg => Select(arg, newTermName("value")))
                      )

                    val typeApplyExprCall =
                      macroExpr(
                        "typeApplyExpr",
                        List(
                          newExpr,
                          Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                          context.literal(select.name.toString).tree,
                          context.literal(select.name.decoded).tree,
                          list(types),
                          list(argsNames)
                        )
                      )

                    Block(List(funValDef) ++ argsValDef, typeApplyExprCall)

                  case _ => fallback(tree) // TODO: Not sure what to do here
                }

            case _ => fallback(tree) // TODO: Not sure what to do here
          }

        case typeApply: TypeApply =>
          typeApply.fun match {
            case select: Select =>
              val funValDef = valDef("$org_scalautils_macro_apply_qualifier", transformAst(select.qualifier))
              val types = typeApply.args.map(t => context.literal(show(t)).tree)
              val newExpr = TypeApply(selectQualifierValue(select), typeApply.args)

              val typeApplyExprCall =
                macroExpr(
                  "typeApplyExpr",
                  List(
                    newExpr,
                    Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                    context.literal(select.name.toString).tree,
                    context.literal(select.name.decoded).tree,
                    list(types),
                    list(List.empty)
                  )
                )

              Block(List(funValDef), typeApplyExprCall)

            case _ => fallback(tree) // TODO: Not sure what to do here
          }

        case select: Select =>
          val qualifierValDef = valDef("$org_scalautils_macro_apply_qualifier", transformAst(select.qualifier))
          val newExpr = selectQualifierValue(select)
            
          Block(
            List(qualifierValDef),
            macroExpr(
              "selectExpr",
              List(
                newExpr,
                Ident(newTermName("$org_scalautils_macro_apply_qualifier")),
                context.literal(select.name.toString).tree,
                context.literal(select.name.decoded).tree
              )
            )
          )

        case ident: Ident =>
          macroExpr(
            "identExpr",
            List(
              tree,
              context.literal(ident.name.toString).tree,
              context.literal(ident.name.decoded).tree
            )
          )

        case thisTree: This =>
          macroExpr("thisExpr", List(tree))

        case block: Block  =>
          Block(
            block.stats,
            transformAst(block.expr)
          )

        case _ => fallback(tree)
      }

    val block = transformAst(expr.tree)
    context.Expr(block)
  }

}

private[scalautils] case class TypeApplyMacroExpr[T](value: T, qualifier: Any, name: String, decodedName: String, types: List[String], args: List[Any]) extends MacroExpr[T] {

  private def notNested(expr: Any): Boolean =
    expr match {
      case apply: ApplyMacroExpr[_] => false
      case _ => true
    }

  private val symbolicSet = Set("*", "/", "%", "+", "-", ":", "=", "!", "<", ">", "&", "^", "|")

  private def isSymbolic: Boolean = symbolicSet.exists(e => decodedName.startsWith(e))

  private def isSingleNotNested: Boolean = args.length == 1 && notNested(args(0))

  private def bracketIfNested(expr: Any): String =
    expr match {
      case apply: ApplyMacroExpr[_] => "(" + Prettifier.default(expr) + ")"
      case _ => Prettifier.default(expr)
    }

  private def isRhsApply: Boolean = decodedName.endsWith(":")

  private def typeParameters: String =
    if (types.length > 0)
      "[" + types.mkString(", ") + "]"
    else
      ""

  override def toString: String =
    if (isSymbolic) {
      val arguments =
        if (isSingleNotNested)
          " " + args.map(Prettifier.default(_)).mkString(", ")
        else if (args.length == 0)
          ""
        else
          " (" + args.map(Prettifier.default(_)).mkString(", ") + ")"

      if (isRhsApply)
        arguments.trim + " " + decodedName + typeParameters + " " + bracketIfNested(qualifier)
      else
        bracketIfNested(qualifier) + " " + decodedName + typeParameters + arguments
    }
    else {
      val arguments =
        if (args.length == 0)
          ""
        else
          "(" + args.map(Prettifier.default(_)).mkString(", ") + ")"
      Prettifier.default(qualifier) + "." + decodedName + typeParameters + arguments
    }
}

private[scalautils] class ApplyMacroExpr[T](value: T, qualifier: Any, name: String, decodedName: String, args: List[Any]) extends
  TypeApplyMacroExpr[T](value, qualifier, name, decodedName, List.empty, args)

private[scalautils] case class SelectMacroExpr[T](value: T, qualifier: Any, name: String, decodedName: String) extends MacroExpr[T] {

  override def toString: String =
    qualifier match {
      case thisExpr: ThisExpr[_] => Prettifier.default(value)
      case _ =>
        if (decodedName.startsWith("unary_"))
          decodedName.substring(6) +  Prettifier.default(qualifier)
        else
          Prettifier.default(qualifier) + "." + decodedName
    }
}

private[scalautils] case class IdentMacroExpr[T](value: T, name: String, decodedName: String) extends MacroExpr[T] {
  override def toString: String = Prettifier.default(value)
}

private[scalautils] case class ThisExpr[T](value: T) extends MacroExpr[T] {
  override def toString: String = "" // omit this in printing
}

private[scalautils] case class FallbackExpr[T](value: T, expressionText: String) extends MacroExpr[T] {
  override def toString: String = Prettifier.default(value)
}