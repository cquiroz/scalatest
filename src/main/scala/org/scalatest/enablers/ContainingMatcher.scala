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

import org.scalatest.matchers.AMatcher

trait ContainingMatcher[T, -C[T]] {

  def containsAMatcher(container: C[T], aMatcher: AMatcher[T]): Boolean

}

object ContainingMatcher {

  implicit def containingNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e]]: ContainingMatcher[E, TRAV] =
    new ContainingMatcher[E, TRAV] {
      def containsAMatcher(container: TRAV[E], aMatcher: AMatcher[E]): Boolean =
        container.exists(aMatcher(_).matches)
    }

}