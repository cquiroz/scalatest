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
package org.scalatest.words

import scala.collection.GenTraversable
import org.scalactic.enablers.ContainingConstraint
import org.scalactic.enablers.AggregatingConstraint
import org.scalactic.enablers.SequencingConstraint
import org.scalatest.enablers.KeyMapping
import org.scalatest.enablers.ValueMapping
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalactic.{Prettifier, Every}

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
class ResultOfContainWord[L](left: L, shouldBeTrue: Boolean = true) {

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain oneOf (1, 2)
   *                   ^
   * </pre>
   */
  def oneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit evidence: ContainingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.oneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "oneOf"))
    if (evidence.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain oneElementOf List(1, 2)
   *                   ^
   * </pre>
   */
  def oneElementOf[R](elements: GenTraversable[R])(implicit evidence: ContainingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.oneElementOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "oneElementOf"))
    if (evidence.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainOneElementOf(left, right) 
        else 
          FailureMessages.containedOneElementOf(left, right)
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atLeastOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atLeastOneOf"))
    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain atLeastOneElementOf List(1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneElementOf[R](elements: GenTraversable[R])(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atLeastOneElementOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atLeastOneElementOf"))
    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAtLeastOneElementOf(left, right) 
        else 
          FailureMessages.containedAtLeastOneElementOf(left, right)
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain noElementsOf List(1, 2)
   *                   ^
   * </pre>
   */
  def noElementsOf[R](elements: GenTraversable[R])(implicit containing: ContainingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.noElementsOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "noElementsOf"))
    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.containedAtLeastOneOf(left, right) 
        else 
          FailureMessages.didNotContainAtLeastOneOf(left, right)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain noneOf (1, 2)
   *                   ^
   * </pre>
   */
  def noneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit containing: ContainingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.noneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "noneOf"))
    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsAs[R](right: GenTraversable[R])(implicit aggregating: AggregatingConstraint[L, R]) {
    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainSameElements(left, right)
        else
          FailureMessages.containedSameElements(left, right)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsInOrderAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsInOrderAs[R](right: GenTraversable[R])(implicit sequencing: SequencingConstraint[L, R]) {
    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainSameElementsInOrder(left, right)
        else
          FailureMessages.containedSameElementsInOrder(left, right)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain only (1, 2)
   *                   ^
   * </pre>
   */
  def only[R](right: R*)(implicit aggregating: AggregatingConstraint[L, R]) {
    if (right.isEmpty)
      throw new NotAllowedException(FailureMessages.onlyEmpty, getStackDepthFun("ResultOfContainWord.scala", "only"))
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.onlyDuplicate, getStackDepthFun("ResultOfContainWord.scala", "only"))
    if (aggregating.containsOnly(left, right) != shouldBeTrue) {
      val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]])
      throw newTestFailedException(
        if (shouldBeTrue)
          if (withFriendlyReminder)
            FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.didNotContainOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          if (withFriendlyReminder)
            FailureMessages.containedOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.containedOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrderOnly (1, 2)
   *                   ^
   * </pre>
   */
  def inOrderOnly[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit sequencing: SequencingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, getStackDepthFun("ResultOfContainWord.scala", "inOrderOnly"))
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain allOf (1, 2)
   *                   ^
   * </pre>
   */
  def allOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.allOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "allOf"))
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain allElementsOf (1, 2)
   *                   ^
   * </pre>
   */
  def allElementsOf[R](elements: GenTraversable[R])(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.allElementsOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "allElementsOf"))
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllElementsOf(left, right) 
        else 
          FailureMessages.containedAllElementsOf(left, right)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrder (1, 2)
   *                   ^
   * </pre>
   */
  def inOrder[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit sequencing: SequencingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderDuplicate, getStackDepthFun("ResultOfContainWord.scala", "inOrder"))
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain inOrderElementsOf List(1, 2)
   *                   ^
   * </pre>
   */
  def inOrderElementsOf[R](elements: GenTraversable[R])(implicit sequencing: SequencingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderElementsOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "inOrderElementsOf"))
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllElementsOfInOrder(left, right) 
        else 
          FailureMessages.containedAllElementsOfInOrder(left, right)
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain key ("one")
   *                    ^
   * </pre>
   */
  def key(expectedKey: Any)(implicit keyMapping: KeyMapping[L]) {
    if (keyMapping.containsKey(left, expectedKey) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainKey(left, expectedKey)
        else
          FailureMessages.containedKey(left, expectedKey)
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain value ("one")
   *                    ^
   * </pre>
   */
  def value(expectedValue: Any)(implicit valueMapping: ValueMapping[L]) {
    if (valueMapping.containsValue(left, expectedValue) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainValue(left, expectedValue)
        else
          FailureMessages.containedValue(left, expectedValue)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atMostOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atMostOneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atMostOneOf"))
    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          FailureMessages.containedAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain atMostOneElementOf (1, 2)
   *                   ^
   * </pre>
   */
  def atMostOneElementOf[R](elements: GenTraversable[R])(implicit aggregating: AggregatingConstraint[L, R]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atMostOneElementOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atMostOneElementOf"))
    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.didNotContainAtMostOneElementOf(left, right) 
        else 
          FailureMessages.containedAtMostOneElementOf(left, right)
      )
  }
  
  override def toString: String = "ResultOfContainWord(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
}

