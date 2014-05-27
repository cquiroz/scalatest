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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalaTestJs {

  private def copyFile(sourceFile: File, destFile: File) {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(line)
        destWriter.newLine()
      }
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  val skipPackages =
    Set(
      "junit",
      "testng",
      "mock",
      "matchers",
      "prop",
      "selenium"
    )

  val skipFiles =
    Set(
      "org/scalatest/Matchers.scala",
      "org/scalatest/MatchersHelper.scala",
      "org/scalatest/package.scala",
      "org/scalatest/tools/Framework.scala",
      "org/scalatest/tools/ScalaTestFramework.scala",
      "org/scalatest/tools/ScalaTestAntTask.scala",
      "org/scalatest/tools/HtmlReporter.scala",
      "org/scalatest/tools/RunnerJFrame.scala",
      "org/scalatest/tools/EventHolder.scala",
      "org/scalatest/tools/IconEmbellishedListCellRenderer.scala",
      "org/scalatest/tools/ReporterFactory.scala"
    )

  def copyDir(targetDir: File, packagePath: String, sourceType: String) {
    val packageDir = new File(targetDir, packagePath)
    packageDir.mkdirs()
    val sourceDir = new File("src/main/" + sourceType + "/" + packagePath)
    sourceDir.listFiles.foreach { sourceFile =>
      if (sourceFile.isDirectory) {
        if (!skipPackages.contains(sourceFile.getName))
          copyDir(targetDir, packagePath + File.separator + sourceFile.getName, sourceType)
      }
      else {
        if (!skipFiles.contains(packagePath + File.separator + sourceFile.getName)) {
          val destFile = new File(packageDir, sourceFile.getName)
          copyFile(sourceFile, destFile)
        }
      }
    }
  }

  def genMainJava(targetDir: File) {
    copyDir(targetDir, "org/scalatest", "java")
  }

  def genMainScala(targetDir: File, scalaVersion: String) {

    copyDir(targetDir, "org/scalatest", "scala")
    copyDir(targetDir, "org/scalactic", "scala")

    val sourceScalaTestResourceFile = new File("src/main/resources/org/scalatest/ScalaTestBundle.properties")
    val destScalaTestResourceDir = new File(targetDir.getParentFile, "resources/org/scalatest")
    destScalaTestResourceDir.mkdirs()
    val destScalaTestResourceFile = new File(destScalaTestResourceDir, "ScalaTestBundle.properties")
    copyFile(sourceScalaTestResourceFile, destScalaTestResourceFile)

    val sourceScalacticResourceFile = new File("src/main/resources/org/scalactic/ScalacticBundle.properties")
    val destScalacticResourceDir = new File(targetDir.getParentFile, "resources/org/scalactic")
    destScalacticResourceDir.mkdirs()
    val destScalacticResourceFile = new File(destScalacticResourceDir, "ScalacticBundle.properties")
    copyFile(sourceScalacticResourceFile, destScalacticResourceFile)

    val sourceCssFile = new File("src/main/html/addl.css")
    val destCssDir = new File(targetDir.getParentFile, "html")
    destCssDir.mkdirs()
    val destCssFile = new File(destCssDir, "addl.css")
    copyFile(sourceCssFile, destCssFile)
  }

  def genTest(targetDir: File, scalaVersion: String) {
    /*val scalatestDir = new File(targetDir, "org/scalatest")
    scalatestDir.mkdirs()
    val sharedHelpersSourceFile = new File("src/test/scala/org/scalatest/SharedHelpers.scala")
    val sharedHelpersTargetFile = new File(scalatestDir, sharedHelpersSourceFile.getName)
    copyFile(sharedHelpersSourceFile, sharedHelpersTargetFile)

    val packageDir = new File(targetDir, "org/scalactic")
    packageDir.mkdirs()
    val sourceDir = new File("src/test/scala/org/scalactic")
    sourceDir.listFiles.foreach { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }*/
  }


}