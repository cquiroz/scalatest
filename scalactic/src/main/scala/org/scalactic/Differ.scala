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
package org.scalactic

trait Differ[-T] {

  def difference(a: T, b: Any): Difference

}

object Differ {

  implicit def default[T] =
    new Differ[T] {
      def difference(a: T, b: Any): Difference =
        new Difference {
          def inlineDiff: Option[(String, String)] =
            (a, b) match {
              case (s1: String, s2: String) => Some(Prettifier.diffStrings(s1, s2))
              case _ => None
            }

          def sideBySideDiff: Option[(String, String)] = None

          def analysis: Option[String] = None
        }
    }
}