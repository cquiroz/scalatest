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
package org.scalatest
package prop

import org.scalactic.anyvals._

class PropertyCheckConfigurationHelperSuite extends FunSuite with Matchers {

  import org.scalatest.prop.Configuration._

  val DefaultMinSuccessful: PosInt = 9
  val PassedMinSuccessful: PosInt = 3

  val DefaultMinSize: PosZInt = 99
  val PassedMinSize: PosZInt = 33

  val DefaultWorkers: PosInt = 99
  val PassedWorkers: PosInt = 33

  val DefaultSizeRange: PosZInt = 0
  val PassedSizeRange: PosZInt = 10

  val DefaultMaxDiscardedFactor: PosZDouble = 1.0
  val PassedMaxDiscardedFactor: PosZDouble = 0.5

  val defaultConfig =
    PropertyCheckConfiguration(
      minSuccessful = DefaultMinSuccessful,
      maxDiscardedFactor = DefaultMaxDiscardedFactor,
      minSize = DefaultMinSize,
      sizeRange = DefaultSizeRange,
      workers = DefaultWorkers
    )

  // minSuccessful
  test("getParameter returns passed minSuccessful config param") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSuccessful should equal (PassedMinSuccessful)
  }

  test("getParameter throws IAE if passed multiple minSuccessful config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MinSuccessful(33), MinSuccessful(34)), defaultConfig)
    }
  }

  test("getParameter returns default minSuccessful config param if none passed") {
    val params = getParameter(Seq(Workers(DefaultWorkers)), defaultConfig)
    params.minSuccessful should equal (DefaultMinSuccessful)
  }

  def maxDiscardRatio(maxDiscardedTests: Int, minSuccessfulTests: Int): Float =
    (maxDiscardedTests: Float)/(minSuccessfulTests: Float)

  // maxDiscarded
  test("getParameter returns passed maxDiscarded config param") {
    val params = getParameter(Seq(MaxDiscardedFactor(PassedMaxDiscardedFactor)), defaultConfig)
    params.maxDiscardedFactor should equal (PassedMaxDiscardedFactor)
  }

  test("getParameter throws IAE if passed multiple maxDiscarded config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MaxDiscardedFactor(33.0), MaxDiscardedFactor(34.0)), defaultConfig)
    }
  }

  test("getParameter returns default maxDiscarded config param if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxDiscardedFactor should equal (DefaultMaxDiscardedFactor)
  }

  // minSize
  test("getParameter returns passed minSize config param") {
    val params = getParameter(Seq(MinSize(PassedMinSize)), defaultConfig)
    params.minSize should equal (PassedMinSize)
  }

  test("getParameter throws IAE if passed multiple minSize config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MinSize(33), MinSize(34)), defaultConfig)
    }
  }

  test("getParameter returns default minSize config param if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSize should equal (DefaultMinSize)
  }

  // sizeRange
  test("getParameter returns passed sizeRange config param") {
    val params = getParameter(Seq(SizeRange(PassedSizeRange)), defaultConfig)
    params.maxSize.value should equal (DefaultMinSize + PassedSizeRange)
  }

  test("getParameter returns passed minSize and sizeRange config param") {
    val params = getParameter(Seq(MinSize(PassedMinSize), SizeRange(PassedSizeRange)), defaultConfig)
    params.maxSize.value should equal (PassedMinSize + PassedSizeRange)
  }

  test("getParameter throws IAE if passed multiple maxSize config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MaxSize(33), MaxSize(34)), defaultConfig)
    }
    intercept[IllegalArgumentException] {
      getParameter(Seq(MaxSize(33), SizeRange(34)), defaultConfig)
    }
    intercept[IllegalArgumentException] {
      getParameter(Seq(SizeRange(33), SizeRange(34)), defaultConfig)
    }
  }

  test("getParameter returns default sizeRange config if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxSize.value should equal (DefaultMinSize + DefaultSizeRange)
  }

  // workers
  test("getParameter returns passed workers config param") {
    val params = getParameter(Seq(Workers(PassedWorkers)), defaultConfig)
    params.workers should equal (PassedWorkers)
  }

  test("getParameter throws IAE if passed multiple workers config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(Workers(33), Workers(34)), defaultConfig)
    }
  }

  test("getParameter returns default workers config param if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.workers should equal (DefaultWorkers)
  }

  test("getParameter returns all default if no config params passed") {
    val params = getParameter(Seq(), defaultConfig)
    params.minSuccessful should equal (DefaultMinSuccessful)
    params.maxDiscardedFactor should equal (DefaultMaxDiscardedFactor)
    params.minSize should equal (DefaultMinSize)
    params.maxSize.value should equal (DefaultMinSize + DefaultSizeRange)
    params.workers should equal (DefaultWorkers)
  }

  test("getParameter returns all passed if all config params passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful), MaxDiscardedFactor(PassedMaxDiscardedFactor), MinSize(PassedMinSize),
      SizeRange(PassedSizeRange), Workers(PassedWorkers)), defaultConfig)
    params.minSuccessful should equal (PassedMinSuccessful)
    params.maxDiscardedFactor should equal (PassedMaxDiscardedFactor)
    params.minSize should equal (PassedMinSize)
    params.maxSize.value should equal (PassedMinSize + PassedSizeRange)
    params.workers should equal (PassedWorkers)
  }
}
