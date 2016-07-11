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
package org.scalatest.enablers

/**
  * Supertrait for typeclasses that enable the <code>be defined</code> matcher syntax.
  *
  * <p>
  * A <code>Definition[T]</code> provides access to the "definition nature" of type <code>S</code> in such
  * a way that <code>be defined</code> matcher syntax can be used with type <code>T</code>. A <code>T</code>
  * can be any type for which the concept of being defined makes sense, such as <code>scala.Option</code>. ScalaTest provides
  * implicit implementation for <code>scala.Option</code>. You can enable the <code>be defined</code> matcher syntax on your own
  * type <code>U</code> by defining a <code>Definition[U]</code> for the type and making it available implicitly.
  *
  * <p>
  * ScalaTest provides an implicit <code>Definition</code> instance for <code>scala.Option</code>,
  * arbitary object with <code>isDefined()</code> or <code>isDefined</code> in the <code>Definition</code> companion object.
  * </p>
  */
trait Differ[-T] {

  def diff(item1: T, item2: Any): (Any, Any)

}

object Differ {

  def diffStrings(s: String, t: String): Tuple2[String, String] = {
    def findCommonPrefixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the prefix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(i) != t.charAt(i))
        if (!found)
          i = i + 1
      }
      i
    }
    def findCommonSuffixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the suffix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
        if (!found)
          i = i + 1
      }
      i
    }
    if (s != t) {
      val commonPrefixLength = findCommonPrefixLength(s, t)
      val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
      val prefix = s.substring(0, commonPrefixLength)
      val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
      val sMiddleEnd = s.length - commonSuffixLength
      val tMiddleEnd = t.length - commonSuffixLength
      val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
      val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
      val MaxContext = 20
      val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
      val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
      (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
    }
    else
      (s, t)
  }

  implicit def differOfT[T]: Differ[T] =
    new Differ[T] {
      def diff(item1: T, item2: Any): (Any, Any) = {
        (item1, item2) match {
          case (s1: String, s2: String) => diffStrings(s1, s2)
          case _ => (item1, item2)
        }
      }

    }

}