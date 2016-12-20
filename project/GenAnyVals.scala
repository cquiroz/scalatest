/*
 * Copyright 2001-2011 Artima, Inc.
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

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object GenAnyVals {

  def genMacro(targetDir: File, typeName: String, typeBooleanExpr: String): File = {
    val content =
      s"""/*
        | * Copyright 2001-2016 Artima, Inc.
        | *
        | * Licensed under the Apache License, Version 2.0 (the "License");
        | * you may not use this file except in compliance with the License.
        | * You may obtain a copy of the License at
        | *
        | *     http://www.apache.org/licenses/LICENSE-2.0
        | *
        | * Unless required by applicable law or agreed to in writing, software
        | * distributed under the License is distributed on an "AS IS" BASIS,
        | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
        | * See the License for the specific language governing permissions and
        | * limitations under the License.
        | */
        |package org.scalactic.anyvals
        |
        |import org.scalactic.Resources
        |import reflect.macros.Context
        |
        |private[scalactic] object ${typeName}Macro extends CompileTimeAssertions {
        |
        |  def isValid(i: Int): Boolean = $typeBooleanExpr
        |
        |  def apply(c: Context)(value: c.Expr[Int]): c.Expr[$typeName] = {
        |    val notValidMsg = Resources.notValid$typeName
        |    val notLiteralMsg = Resources.notLiteral$typeName
        |
        |    import c.universe._
        |
        |    ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg)(isValid)
        |    reify { $typeName.ensuringValid(value.splice) }
        |  }
        |}
      """.stripMargin

    val targetFile = new File(targetDir, typeName + "Macro.scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(content)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    targetFile
  }

  def genIntAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                   typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String, widensToTypes: Seq[String]): Seq[File] = {
    val templateSource = scala.io.Source.fromFile("project/templates/IntAnyVal.template")
    val templateText = try templateSource.mkString finally templateSource.close()
    val st = new org.antlr.stringtemplate.StringTemplate(templateText)

    st.setAttribute("typeName", typeName)
    st.setAttribute("typeDesc", typeDesc)
    st.setAttribute("typeNote", typeNote)
    st.setAttribute("typeBooleanExpr", typeBooleanExpr)
    st.setAttribute("typeValidExample", typeValidExample)
    st.setAttribute("typeInvalidExample", typeInvalidExample)
    st.setAttribute("typeMinValue", typeMinValue)
    st.setAttribute("typeMinValueNumber", typeMinValueNumber)
    st.setAttribute("typeMaxValue", typeMaxValue)
    st.setAttribute("typeMaxValueNumber", typeMaxValueNumber)

    val widensToOtherAnyVals =
      widensToTypes.map { targetType =>
        s"""/**
            |   * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
            |   *
            |   * @param pos the <code>$typeName</code> to widen
            |   * @return the <code>$targetType</code> widen from <code>$typeName</code>.
            |   */
            |  implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
            |
        """.stripMargin
      }.mkString

    st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

    val targetFile = new File(targetDir, typeName + ".scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(st.toString)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    List(targetFile, genMacro(targetDir, typeName, typeBooleanExpr))
  }

  def genMain(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()
    genIntAnyVal(dir, "NonZeroInt", "non-zero", "Note: a <code>NonZeroInt</code> may not equal 0.", "i != 0", "NonZeroInt(42)", "NonZeroInt(0)", "Int.MinValue", "-2147483648",
                 "Int.MaxValue", "2147483647", List("NonZeroLong"))
  }

}