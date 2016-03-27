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

import org.scalatest.matchers._
import org.scalatest.enablers._
import scala.collection.GenTraversable
import org.scalactic._
import org.scalactic.TripleEqualsSupport.Spread
import TripleEqualsSupport.TripleEqualsInvocation
import org.scalatest._
import org.scalactic.Equality
import org.scalatest.Assertions.areEqualComparingArraysStructurally
// SKIP-SCALATESTJS-START
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod
// SKIP-SCALATESTJS-END
import scala.annotation.tailrec
import org.scalatest.MatchersHelper.fullyMatchRegexWithGroups
import org.scalatest.MatchersHelper.startWithRegexWithGroups
import org.scalatest.MatchersHelper.endWithRegexWithGroups
import org.scalatest.MatchersHelper.includeRegexWithGroups
import org.scalatest.Suite.getObjectsForFailureMessage
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.Resources
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalactic.TripleEqualsSupport.Spread
import org.scalactic.TripleEqualsSupport.TripleEqualsInvocation

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class NotWord {

  /**
   * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
   * and <code>exist</code> is a <code>Matcher[java.io.File]</code>: 
   *
   * <pre class="stHighlight">
   * tempFile should not (exist)
   *                     ^
   * </pre>
   */
  def apply[S](matcher: Matcher[S]): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = matcher(left).negated
      override def toString: String = "not (" + Prettifier.default(matcher) + ")"
    }

  import scala.language.higherKinds

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * hasNoSize should not { have size (2) and equal (hasNoSize) }
   *                      ^
   * </pre>
   */
  def apply[S, TYPECLASS[_]](matcherGen1: MatcherFactory1[S, TYPECLASS]): MatcherFactory1[S, TYPECLASS] = {
    new MatcherFactory1[S, TYPECLASS] {
      def matcher[V <: S : TYPECLASS]: Matcher[V] = {
        val innerMatcher: Matcher[V] = matcherGen1.matcher
        new Matcher[V] {
          def apply(left: V): MatchResult = innerMatcher(left).negated
          override def toString: String = "not (" + Prettifier.default(matcherGen1) + ")"
        }
      }
      override def toString: String = "not (" + Prettifier.default(matcherGen1) + ")"
    }
  }

  def apply[S, TYPECLASS1[_], TYPECLASS2[_]](matcherGen2: MatcherFactory2[S, TYPECLASS1, TYPECLASS2]): MatcherFactory2[S, TYPECLASS1, TYPECLASS2] = {
    new MatcherFactory2[S, TYPECLASS1, TYPECLASS2] {
      def matcher[V <: S : TYPECLASS1 : TYPECLASS2]: Matcher[V] = {
        val innerMatcher: Matcher[V] = matcherGen2.matcher
        new Matcher[V] {
          def apply(left: V): MatchResult = innerMatcher(left).negated
          override def toString: String = "not (" + Prettifier.default(matcherGen2) + ")"
        }
      }
      override def toString: String = "not (" + Prettifier.default(matcherGen2) + ")"
    }
  }

  /**
   * This method enables any <code>BeMatcher</code> to be negated by passing it to <code>not</code>. 
   * For example, if you have a <code>BeMatcher[Int]</code> called <code>odd</code>, which matches
   * <code>Int</code>s that are odd, you can negate it to get a <code>BeMatcher[Int]</code> that matches
   * even <code>Int</code>s, like this:
   *
   * <pre class="stHighlight">
   * val even = not (odd)
   *                ^
   * </pre>
   *
   * <p>
   * In addition, this method enables you to negate a <code>BeMatcher</code> at its point of use, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * num should be (not (odd))
   * </pre>
   *
   * <p>
   * Nevertheless, in such as case it would be more idiomatic to write:
   * </p>
   *
   * <pre class="stHighlight">
   * num should not be (odd)
   * </pre>
   */
  def apply[S](beMatcher: BeMatcher[S]): BeMatcher[S] =
    new BeMatcher[S] {
      def apply(left: S): MatchResult = beMatcher(left).negated
      override def toString: String = "not (" + Prettifier.default(beMatcher) + ")"
    }
  
  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * file should not (exist)
   *             ^
   * </pre>
   */
  def apply(existWord: ExistWord): ResultOfNotExist = 
    new ResultOfNotExist(this)

  /* 
   * This is used in logical expression like: 
   * outerInstance.and(MatcherWords.not.exist)
   *                                    ^
   */ 
  private[scalatest] def exist(implicit prettifier: Prettifier): MatcherFactory1[Any, Existence] =
    new MatcherFactory1[Any, Existence] {
      def matcher[T : Existence]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val existence = implicitly[Existence[T]]
            MatchResult(
              !existence.exists(left), 
              Resources.rawExists,
              Resources.rawDoesNotExist,
              Vector(left),
              prettifier
            )
          } 
          override def toString: String = "not exist"
        }
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should (not equal (7) and be &lt; (9))
   *                 ^
   * </pre>
   */
  def equal(right: Any): MatcherFactory1[Any, Equality] = apply(MatcherWords.equal(right))

  /**
   * This method enables the following syntax for the "primitive" numeric types: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should ((not equal (17.1 +- 0.2)) and (not equal (27.1 +- 0.2)))
   *                         ^
   * </pre>
   */
  def equal[U](spread: Spread[U])(implicit prettifier: Prettifier): Matcher[U] = {
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        MatchResult(
          !(spread.isWithin(left)),
          Resources.rawEqualedPlusOrMinus,
          Resources.rawDidNotEqualPlusOrMinus,
          Vector(left, spread.pivot, spread.tolerance),
          prettifier
        )
      }
      override def toString: String = "not equal " + Prettifier.default(spread)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should (not equal (null))
   *                 ^
   * </pre>
   */
  def equal(o: Null)(implicit prettifier: Prettifier): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left != null,
          Resources.rawEqualedNull,
          Resources.rawDidNotEqualNull,
          Resources.rawMidSentenceEqualedNull,
          Resources.rawDidNotEqualNull,
          Vector.empty, 
          Vector(left), 
          Vector.empty, 
          Vector(left),
          prettifier
        )
      }
      override def toString: String = "not equal null"
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not have length (5) and not have length (3))
   *                         ^
   * </pre>
   */
  def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Length] =
    apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)(prettifier))

  // This looks similar to the AndNotWord one, but not quite the same because no and
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not have size (5) and not have size (3))
   *                         ^
   * </pre>
   */
  def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[Any, Size] =
    apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize))
    
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should (not have message ("Message from Mars!") and not have message ("Message from Mars!"))
   *                    ^
   * </pre>
   */
  def have(resultOfMessageWordApplication: ResultOfMessageWordApplication): MatcherFactory1[Any, Messaging] =
    apply(MatcherWords.have.message(resultOfMessageWordApplication.expectedMessage))

  /**
   * This method enables the following syntax, where, for example, <code>book</code> is of type <code>Book</code> and <code>title</code> and <code>author</code>
   * are both of type <code>HavePropertyMatcher[Book, String]</code>:
   *
   * <pre class="stHighlight">
   * book should (not have (title ("Moby Dick")) and (not have (author ("Melville"))))
   *                  ^
   * </pre>
   */
  def have[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =
    apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*))

  /**
   * This method enables the following syntax, where, for example, <code>num</code> is an <code>Int</code> and <code>odd</code>
   * of type <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * num should (not be (odd) and be &lt;= (8))
   *                 ^
   * </pre>
   */
  def be[T](beMatcher: BeMatcher[T]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = beMatcher(left).negated
      override def toString: String = "not be " + Prettifier.default(beMatcher)
    }
  }

  import scala.language.experimental.macros

  /**
   * This method enables the following syntax, where, for example, <code>num</code> is an <code>Int</code> and <code>odd</code>
   * of type <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * result should (not matchPattern { case Person("Bob", _)} and equal (result2))
   *                    ^
   * </pre>
   */
  def matchPattern(right: PartialFunction[Any, _])(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[Any] = macro MatchPatternMacro.notMatchPatternMatcher

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should (not be (null))
   *                 ^
   * </pre>
   */
  def be(o: Null)(implicit prettifier: Prettifier): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left != null,
          Resources.rawWasNull,
          Resources.rawWasNotNull,
          Resources.rawMidSentenceWasNull,
          Resources.rawWasNotNull,
          Vector.empty, 
          Vector(left), 
          Vector.empty, 
          Vector(left),
          prettifier
        )
      }
      override def toString: String = "not be null"
    }

  // These next four are for things like not be </>/<=/>=:
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be < (7) and not be > (10))
   *                 ^
   * </pre>
   */
  def be[T](resultOfLessThanComparison: ResultOfLessThanComparison[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          !resultOfLessThanComparison(left),
          Resources.rawWasLessThan,
          Resources.rawWasNotLessThan,
          Vector(left, resultOfLessThanComparison.right),
          prettifier
        )
      override def toString: String = "not be " + Prettifier.default(resultOfLessThanComparison)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be > (10) and not be < (7))
   *                 ^
   * </pre>
   */
  def be[T](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          !resultOfGreaterThanComparison(left),
          Resources.rawWasGreaterThan,
          Resources.rawWasNotGreaterThan,
          Vector(left, resultOfGreaterThanComparison.right),
          prettifier
        )
      override def toString: String = "not be " + Prettifier.default(resultOfGreaterThanComparison)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be <= (7) and not be > (10))
   *                 ^
   * </pre>
   */
  def be[T](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          !resultOfLessThanOrEqualToComparison(left),
          Resources.rawWasLessThanOrEqualTo,
          Resources.rawWasNotLessThanOrEqualTo,
          Vector(left, resultOfLessThanOrEqualToComparison.right),
          prettifier
        )
      override def toString: String = "not be " + Prettifier.default(resultOfLessThanOrEqualToComparison)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be >= (10) and not be < (7))
   *                 ^
   * </pre>
   */
  def be[T](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          !resultOfGreaterThanOrEqualToComparison(left),
          Resources.rawWasGreaterThanOrEqualTo,
          Resources.rawWasNotGreaterThanOrEqualTo,
          Vector(left, resultOfGreaterThanOrEqualToComparison.right),
          prettifier
        )
      override def toString: String = "not be " + Prettifier.default(resultOfGreaterThanOrEqualToComparison)
    }
  }

  /**
   * <strong>
   * The deprecation period for the "be ===" syntax has expired, and the syntax 
   * will now throw <code>NotAllowedException</code>.  Please use should equal, should ===, shouldEqual,
   * should be, or shouldBe instead.
   * </strong>
   *
   * <p>
   * Note: usually syntax will be removed after its deprecation period. This was left in because otherwise the syntax could in some
   * cases still compile, but silently wouldn't work.
   * </p>
   */
  @deprecated("The deprecation period for the be === syntax has expired. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.")
  def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[Any] = {
    throw new NotAllowedException(FailureMessages.beTripleEqualsNotAllowed,
                                  getStackDepthFun("NotWord.scala", "be")) 
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * myFile should (not be ('hidden) and have (name ("temp.txt")))
   *                    ^
   * </pre>
   */
  def be[T <: AnyRef](symbol: Symbol)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val positiveMatchResult = matchSymbolToPredicateMethod(left, symbol, false, false)(prettifier, sourceInfo)
        MatchResult(
          !positiveMatchResult.matches,
          positiveMatchResult.rawNegatedFailureMessage,
          positiveMatchResult.rawFailureMessage, 
          positiveMatchResult.negatedFailureMessageArgs, 
          positiveMatchResult.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(symbol)
    }
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
   * and <code>hidden</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
   *
   * <pre class="stHighlight">
   * tempFile should (not be (hidden) and have ('name ("temp.txt")))
   *                    ^
   * </pre>
   */
  def be[T <: AnyRef](bePropertyMatcher: BePropertyMatcher[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          !result.matches,
          Resources.rawWas,
          Resources.rawWasNot,
          Vector(left, UnquotedString(result.propertyName)),
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(bePropertyMatcher)
    }
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * isNotFileMock should (not be a ('file) and have ('name ("temp.txt"))))
   *                           ^
   * </pre>
   */
  def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)(prettifier, sourceInfo)
        MatchResult(
          !positiveMatchResult.matches,
          positiveMatchResult.rawNegatedFailureMessage,
          positiveMatchResult.rawFailureMessage, 
          positiveMatchResult.negatedFailureMessageArgs, 
          positiveMatchResult.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(resultOfAWordApplication)
    }
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>notSoSecretFile</code>, for example, refers to a <code>java.io.File</code>
   * and <code>directory</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
   *
   * <pre class="stHighlight">
   * notSoSecretFile should (not be a (directory) and have ('name ("passwords.txt")))
   *                             ^
   * </pre>
   */
  def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = resultOfAWordApplication.bePropertyMatcher(left)
        MatchResult(
          !result.matches,
          Resources.rawWasA,
          Resources.rawWasNotA,
          Vector(left, UnquotedString(result.propertyName)),
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(resultOfAWordApplication)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should (not be a (passedMarks) and be a (validMarks)))
   *                    ^
   * </pre>
   */
  def be[T](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = resultOfAWordApplication.aMatcher(left)
        MatchResult(
          !result.matches,
          result.rawNegatedFailureMessage,
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(resultOfAWordApplication)
    }
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * isNotAppleMock should (not be an ('apple) and not be ('rotten))
   *                            ^
   * </pre>
   */
  def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)(prettifier, sourceInfo)
        MatchResult(
          !positiveMatchResult.matches,
          positiveMatchResult.rawNegatedFailureMessage,
          positiveMatchResult.rawFailureMessage, 
          positiveMatchResult.negatedFailureMessageArgs, 
          positiveMatchResult.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(resultOfAnWordApplication)
    }
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * myFile should (not be an (directory) and not be an (directory))
   *                    ^
   * </pre>
   */
  def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = resultOfAnWordApplication.bePropertyMatcher(left)
        MatchResult(
          !result.matches,
          Resources.rawWasAn,
          Resources.rawWasNotAn,
          Vector(left, UnquotedString(result.propertyName)),
          prettifier
        )
      }
      override def toString: String = "not be " + prettifier(resultOfAnWordApplication)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should (not be a (passedMarks) and be a (validMarks)))
   *                    ^
   * </pre>
   */
  def be[T](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = resultOfAnWordApplication.anMatcher(left)
        MatchResult(
          !result.matches,
          result.rawNegatedFailureMessage,
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not be " + Prettifier.default(resultOfAnWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * myFish should (not be theSameInstanceAs (redFish) and not be theSameInstanceAs (blueFish))
   *                    ^
   * </pre>
   */
  def be[T <: AnyRef](resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit prettifier: Prettifier): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          resultOfTheSameInstanceAsApplication.right ne left,
          Resources.rawWasSameInstanceAs,
          Resources.rawWasNotSameInstanceAs,
          Vector(left, resultOfTheSameInstanceAsApplication.right),
          prettifier
        )
      }
      override def toString: String = "not be " + Prettifier.default(resultOfTheSameInstanceAsApplication)
    }
  }

  /**
   * This method enables the following syntax for the "primitive" numeric types: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should ((not be (17.1 +- 0.2)) and (not be (27.1 +- 0.2)))
   *                         ^
   * </pre>
   */
  def be[U](spread: Spread[U])(implicit prettifier: Prettifier): Matcher[U] = {
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        MatchResult(
          !(spread.isWithin(left)),
          Resources.rawWasPlusOrMinus,
          Resources.rawWasNotPlusOrMinus,
          Vector(left, spread.pivot, spread.tolerance),
          prettifier
        )
      }
      override def toString: String = "not be " + Prettifier.default(spread)
    }
  }
  
  /**
   * This method enables the following syntax, where fraction is a <code>PartialFunction</code>:
   *
   * <pre class="stHighlight">
   * fraction should (not be definedAt (8) and not be definedAt (0))
   *                      ^
   * </pre>
   */
  def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A])(implicit prettifier: Prettifier): Matcher[U] = {
    new Matcher[U] {
      def apply(left: U): MatchResult =
        MatchResult(
          !(left.isDefinedAt(resultOfDefinedAt.right)),
          Resources.rawWasDefinedAt,
          Resources.rawWasNotDefinedAt,
          Vector(left, resultOfDefinedAt.right),
          prettifier
        )
      override def toString: String = "not be " + prettifier(resultOfDefinedAt)
    }
  }

  /**
   * This method enables <code>be</code> to be used for inequality comparison. Here are some examples:
   *
   * <pre class="stHighlight">
   * result should not be (None)
   *                      ^
   * result should not be (Some(1))
   *                      ^
   * result should not be (true)
   *                      ^
   * result should not be (false)
   *                      ^
   * sum should not be (19)
   *                   ^
   * </pre>
   */
  def be(right: Any)(implicit prettifier: Prettifier): Matcher[Any] = {
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        left match {
          case null =>
            MatchResult(
              right != null, 
              Resources.rawWasNull,
              Resources.rawWasNotNull,
              Resources.rawMidSentenceWasNull,
              Resources.rawWasNotNull,
              Vector.empty, 
              Vector(right),
              prettifier
            )
          case _ => 
            val (leftee, rightee) = getObjectsForFailureMessage(left, right) // TODO: To move this to reporter
            MatchResult(
              !areEqualComparingArraysStructurally(left, right),
              Resources.rawWasEqualTo,
              Resources.rawWasNotEqualTo,
              Vector(left, right), 
              Vector(leftee, rightee),
              prettifier
            )
        }
      }
      override def toString: String = "not be " + prettifier(right)
    }
  }
  
  /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fraction should (not be sorted and not be sorted)
     *                      ^
     * </pre>
     */
  def be[T](sortedWord: SortedWord): MatcherFactory1[Any, Sortable] =
    apply(MatcherWords.be(sortedWord))
    
  /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fraction should (not be readable and not equal readableFile)
     *                      ^
     * </pre>
     */
  def be(readableWord: ReadableWord): MatcherFactory1[Any, Readability] =
    apply(MatcherWords.be(readableWord))
  
  /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fraction should (not be writable and not be writableFile)
     *                      ^
     * </pre>
     */
  def be(writableWord: WritableWord): MatcherFactory1[Any, Writability] =
    apply(MatcherWords.be(writableWord))
    
  /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * nonEmptyList should (not be empty and not equal emptyList)
     *                          ^
     * </pre>
     */
  def be(emptyWord: EmptyWord): MatcherFactory1[Any, Emptiness] =
    apply(MatcherWords.be(emptyWord))
    
  /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be defined and not equal something)
     *                    ^
     * </pre>
     */
  def be(definedWord: DefinedWord): MatcherFactory1[Any, Definition] =
    apply(MatcherWords.be(definedWord))
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should (not be a [Book] and not be sorted)
   *                    ^
   * </pre>
   */
  def be(aType: ResultOfATypeInvocation[_])(implicit prettifier: Prettifier, sourceInfo: SourceInfo) = macro TypeMatcherMacro.notATypeMatcher
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should (not be an [Book] and not be sorted)
   *                    ^
   * </pre>
   */
  def be(anType: ResultOfAnTypeInvocation[_])(implicit prettifier: Prettifier, sourceInfo: SourceInfo) = macro TypeMatcherMacro.notAnTypeMatcher

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not fullyMatch regex ("Hel*o") and not include ("orld"))
   *                    ^
   * </pre>
   */
  def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit prettifier: Prettifier): Matcher[String] = {
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        val result = fullyMatchRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups, prettifier)
        MatchResult(
          !result.matches, 
          result.rawNegatedFailureMessage, 
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not fullyMatch " + prettifier(resultOfRegexWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not include regex ("Hel.o") and not include regex ("""(-)?(\d+)(\.\d*)?"""))
   *                    ^
   * </pre>
   */
  def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit prettifier: Prettifier): Matcher[String] = {
    val rightRegex = resultOfRegexWordApplication.regex
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        val result = includeRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups, prettifier)
        MatchResult(
          !result.matches, 
          result.rawNegatedFailureMessage, 
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not include " + prettifier(resultOfRegexWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not include ("cat") and not include ("1.7"))
   *                    ^
   * </pre>
   */
  def include(expectedSubstring: String)(implicit prettifier: Prettifier): Matcher[String] = {
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          !(left.indexOf(expectedSubstring) >= 0), 
          Resources.rawIncludedSubstring,
          Resources.rawDidNotIncludeSubstring,
          Vector(left, expectedSubstring),
          prettifier
        )
      override def toString: String = "not include " + prettifier(expectedSubstring)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not startWith regex ("hel*o") and not endWith regex ("wor.d"))
   *                    ^
   * </pre>
   */
  def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit prettifier: Prettifier): Matcher[String] = {
    val rightRegex = resultOfRegexWordApplication.regex
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        val result = startWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups, prettifier)
        MatchResult(
          !result.matches, 
          result.rawNegatedFailureMessage, 
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not startWith " + prettifier(resultOfRegexWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should ((not startWith ("red")) and (not startWith ("1.7")))
   *                     ^
   * </pre>
   */
  def startWith(expectedSubstring: String)(implicit prettifier: Prettifier): Matcher[String] = {
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          left.indexOf(expectedSubstring) != 0,
          Resources.rawStartedWith,
          Resources.rawDidNotStartWith,
          Vector(left, expectedSubstring),
          prettifier
        )
      override def toString: String = "not startWith " + prettifier(expectedSubstring)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not endWith regex ("wor.d") and not startWith regex ("Hel*o"))
   *                    ^
   * </pre>
   */
  def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit prettifier: Prettifier): Matcher[String] = {
    val rightRegex = resultOfRegexWordApplication.regex
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        val result = endWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups, prettifier)
        MatchResult(
          !result.matches, 
          result.rawNegatedFailureMessage, 
          result.rawFailureMessage, 
          result.negatedFailureMessageArgs, 
          result.failureMessageArgs,
          prettifier
        )
      }
      override def toString: String = "not endWith " + prettifier(resultOfRegexWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should (not endWith ("blue") and not endWith ("1.7"))
   *                    ^
   * </pre>
   */
  def endWith(expectedSubstring: String)(implicit prettifier: Prettifier): Matcher[String] = {
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        MatchResult(
          !(left endsWith expectedSubstring),
          Resources.rawEndedWith,
          Resources.rawDidNotEndWith,
          Vector(left, expectedSubstring),
          prettifier
        )
      }
      override def toString: String = "not endWith " + prettifier(expectedSubstring)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * list should (not contain (null))
   *                  ^
   * </pre>
   */
  def contain(nullValue: Null)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[U : Containing]: Matcher[U] =
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            MatchResult(
              !containing.contains(left, null),
              Resources.rawContainedNull,
              Resources.rawDidNotContainNull,
              Vector(left),
              prettifier
            )
          }
          override def toString: String = "not contain null"
        }
      override def toString: String = "not contain null"
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain (5) and not contain (3))
   *                         ^
   * </pre>
   */
  def contain[T](expectedElement: T)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[U : Containing]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            MatchResult(
              !containing.contains(left, expectedElement),
              Resources.rawContainedExpectedElement,
              Resources.rawDidNotContainExpectedElement,
              Vector(left, expectedElement),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(expectedElement)
        }
      override def toString: String = "not contain " + prettifier(expectedElement)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain oneOf (5, 6, 7))
   *                         ^
   * </pre>
   */
  def contain[T](oneOf: ResultOfOneOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = oneOf.right

            MatchResult(
              !containing.containsOneOf(left, right),
              Resources.rawContainedOneOfElements,
              Resources.rawDidNotContainOneOfElements,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(oneOf)
        }
      }
      override def toString: String = "not contain " + prettifier(oneOf)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain oneElementOf (List(5, 6, 7)))
   *                         ^
   * </pre>
   */
  def contain[T](oneElementOf: ResultOfOneElementOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = oneElementOf.right

            MatchResult(
              !containing.containsOneOf(left, right.distinct),
              Resources.rawContainedOneElementOf,
              Resources.rawDidNotContainOneElementOf,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(oneElementOf)
        }
      }
      override def toString: String = "not contain " + prettifier(oneElementOf)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain (5) and not contain (3))
   *                         ^
   * </pre>
   */
  def contain[T](atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = atLeastOneOf.right

            MatchResult(
              !aggregating.containsAtLeastOneOf(left, right),
              Resources.rawContainedAtLeastOneOf,
              Resources.rawDidNotContainAtLeastOneOf,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(atLeastOneOf)
        }
      }
      override def toString: String = "not contain " + prettifier(atLeastOneOf)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain atLeastOneElementOf List(1, 2, 3))
   *                         ^
   * </pre>
   */
  def contain[T](atLeastOneElementOf: ResultOfAtLeastOneElementOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = atLeastOneElementOf.right

            MatchResult(
              !aggregating.containsAtLeastOneOf(left, right),
              Resources.rawContainedAtLeastOneElementOf,
              Resources.rawDidNotContainAtLeastOneElementOf,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(atLeastOneElementOf)
        }
      }
      override def toString: String = "not contain " + prettifier(atLeastOneElementOf)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain noneOf (5, 6, 7))
   *                         ^
   * </pre>
   */
  def contain[T](noneOf: ResultOfNoneOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = noneOf.right

            MatchResult(
              !containing.containsNoneOf(left, right),
              Resources.rawDidNotContainAtLeastOneOf,
              Resources.rawContainedAtLeastOneOf,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(noneOf)
        }
      }
      override def toString: String = "not contain " + prettifier(noneOf)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain noElementsOf (5, 6, 7))
   *                         ^
   * </pre>
   */
  def contain[T](noElementsOf: ResultOfNoElementsOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = noElementsOf.right

            MatchResult(
              !containing.containsNoneOf(left, right.distinct),
              Resources.rawDidNotContainAtLeastOneElementOf,
              Resources.rawContainedAtLeastOneElementOf,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(noElementsOf)
        }
      }
      override def toString: String = "not contain " + prettifier(noElementsOf)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain theSameElementsAs (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](theSameElementAs: ResultOfTheSameElementsAsApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = theSameElementAs.right

            MatchResult(
              !aggregating.containsTheSameElementsAs(left, right),
              Resources.rawContainedSameElements,
              Resources.rawDidNotContainSameElements,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(theSameElementAs)
        }
      }
      override def toString: String = "not contain " + prettifier(theSameElementAs)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain theSameElementsInOrderAs (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](theSameElementInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = theSameElementInOrderAs.right

            MatchResult(
              !sequencing.containsTheSameElementsInOrderAs(left, right),
              Resources.rawContainedSameElementsInOrder,
              Resources.rawDidNotContainSameElementsInOrder,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(theSameElementInOrderAs)
        }
      }
      override def toString: String = "not contain " + prettifier(theSameElementInOrderAs)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain only (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](only: ResultOfOnlyApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = only.right

            val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]])

            MatchResult(
              !aggregating.containsOnly(left, right),
              if (withFriendlyReminder) Resources.rawContainedOnlyElementsWithFriendlyReminder else Resources.rawContainedOnlyElements,
              if (withFriendlyReminder) Resources.rawDidNotContainOnlyElementsWithFriendlyReminder else Resources.rawDidNotContainOnlyElements,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(only)
        }
      }
      override def toString: String = "not contain " + prettifier(only)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain only (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](inOrderOnly: ResultOfInOrderOnlyApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = inOrderOnly.right

            MatchResult(
              !sequencing.containsInOrderOnly(left, right),
              Resources.rawContainedInOrderOnlyElements,
              Resources.rawDidNotContainInOrderOnlyElements,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(inOrderOnly)
        }
      }
      override def toString: String = "not contain " + prettifier(inOrderOnly)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain allOf (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](allOf: ResultOfAllOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = allOf.right

            MatchResult(
              !aggregating.containsAllOf(left, right),
              Resources.rawContainedAllOfElements,
              Resources.rawDidNotContainAllOfElements,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(allOf)
        }
      }
      override def toString: String = "not contain " + prettifier(allOf)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain allOf (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain(allElementsOf: ResultOfAllElementsOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = allElementsOf.right

            MatchResult(
              !aggregating.containsAllOf(left, right.distinct),
              Resources.rawContainedAllElementsOf,
              Resources.rawDidNotContainAllElementsOf,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(allElementsOf)
        }
      }
      override def toString: String = "not contain " + prettifier(allElementsOf)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain inOrder (1, 2, 3) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain[T](inOrder: ResultOfInOrderApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = inOrder.right

            MatchResult(
              !sequencing.containsInOrder(left, right),
              Resources.rawContainedAllOfElementsInOrder,
              Resources.rawDidNotContainAllOfElementsInOrder,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(inOrder)
        }
      }
      override def toString: String = "not contain " + prettifier(inOrder)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain inOrderElementsOf (List(1, 2, 3)) and not contain (3))
   *                                 ^
   * </pre>
   */
  def contain(inOrderElementsOf: ResultOfInOrderElementsOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = inOrderElementsOf.right

            MatchResult(
              !sequencing.containsInOrder(left, right.distinct),
              Resources.rawContainedAllElementsOfInOrder,
              Resources.rawDidNotContainAllElementsOfInOrder,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(inOrderElementsOf)
        }
      }
      override def toString: String = "not contain " + prettifier(inOrderElementsOf)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain atMostOneOf (5) and not contain (3))
   *                         ^
   * </pre>
   */
  def contain[T](atMostOneOf: ResultOfAtMostOneOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            val right = atMostOneOf.right

            MatchResult(
              !aggregating.containsAtMostOneOf(left, right),
              Resources.rawContainedAtMostOneOf,
              Resources.rawDidNotContainAtMostOneOf,
              Vector(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(atMostOneOf)
        }
      }
      override def toString: String = "not contain " + prettifier(atMostOneOf)
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain atMostOneElementOf (List(5)) and not contain (3))
   *                         ^
   * </pre>
   */
  def contain(atMostOneElementOf: ResultOfAtMostOneElementOfApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {

            val right = atMostOneElementOf.right

            MatchResult(
              !aggregating.containsAtMostOneOf(left, right.distinct),
              Resources.rawContainedAtMostOneElementOf,
              Resources.rawDidNotContainAtMostOneElementOf,
              Vector(left, right),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(atMostOneElementOf)
        }
      }
      override def toString: String = "not contain " + prettifier(atMostOneElementOf)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain key ("three"))
   *                                         ^
   * </pre>
   */
  def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, KeyMapping] = {
    new MatcherFactory1[Any, KeyMapping] {
      def matcher[T](implicit keyMapping: KeyMapping[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val expectedKey = resultOfKeyWordApplication.expectedKey
            MatchResult(
              !keyMapping.containsKey(left, expectedKey),
              Resources.rawContainedKey,
              Resources.rawDidNotContainKey,
              Vector(left, expectedKey),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(resultOfKeyWordApplication)
        }
      }
      override def toString: String = "not contain " + prettifier(resultOfKeyWordApplication)
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (3))
   *                                         ^
   * </pre>
   */
  def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit prettifier: Prettifier): MatcherFactory1[Any, ValueMapping] = {
    new MatcherFactory1[Any, ValueMapping] {
      def matcher[T](implicit valueMapping: ValueMapping[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val expectedValue = resultOfValueWordApplication.expectedValue
            MatchResult(
              !valueMapping.containsValue(left, expectedValue),
              Resources.rawContainedValue,
              Resources.rawDidNotContainValue,
              Vector(left, expectedValue),
              prettifier
            )
          }
          override def toString: String = "not contain " + prettifier(resultOfValueWordApplication)
        }
      }
      override def toString: String = "not contain " + prettifier(resultOfValueWordApplication)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should (not contain a (passedMarks) and contain a (validMarks)))
   *                    ^
   * </pre>
   */
  private[scalatest] def contain[T](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[GenTraversable[T]] = {
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
        val aMatcher = resultOfAWordApplication.aMatcher
        val matched = left.find(aMatcher(_).matches)
        MatchResult(
          !matched.isDefined, 
          Resources.rawContainedA,
          Resources.rawDidNotContainA,
          Vector(left, UnquotedString(aMatcher.nounName), UnquotedString(if (matched.isDefined) aMatcher(matched.get).negatedFailureMessage else "-")), 
          Vector(left, UnquotedString(aMatcher.nounName)),
          prettifier
        )
      }
      override def toString: String = "not contain " + prettifier(resultOfAWordApplication)
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should (not contain an (passedMarks) and contain an (validMarks)))
   *                    ^
   * </pre>
   */
  private[scalatest] def contain[T](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[T])(implicit prettifier: Prettifier): Matcher[GenTraversable[T]] = {
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
        val anMatcher = resultOfAnWordApplication.anMatcher
        val matched = left.find(anMatcher(_).matches)
        MatchResult(
          !matched.isDefined, 
          Resources.rawContainedAn,
          Resources.rawDidNotContainAn,
          Vector(left, UnquotedString(anMatcher.nounName), UnquotedString(if (matched.isDefined) anMatcher(matched.get).negatedFailureMessage else "-")), 
          Vector(left, UnquotedString(anMatcher.nounName)),
          prettifier
        )
      }
      override def toString: String = "not contain " + prettifier(resultOfAnWordApplication)
    }
  }
  
  /**
   * Overrides toString to return "not"
   */
  override def toString: String = "not"
}

