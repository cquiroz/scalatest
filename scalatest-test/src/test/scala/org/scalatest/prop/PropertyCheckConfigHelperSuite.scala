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

@deprecated("Remove when removing PropertyCheckConfig")
class PropertyCheckConfigHelperSuite extends FunSuite with Matchers {

  import Configuration._

  val DefaultMinSuccessful: PosInt = 9
  val PassedMinSuccessful: PosInt = 3

  val DefaultMaxDiscarded = 99
  val PassedMaxDiscarded = 33

  val DefaultMinSize: PosZInt = 99
  val PassedMinSize: PosZInt = 33

  val DefaultMaxSize: PosZInt = 99
  val PassedMaxSize: PosZInt = 33

  val DefaultWorkers: PosInt = 99
  val PassedWorkers: PosInt = 33

  val defaultConfig =
    PropertyCheckConfig(
      minSuccessful = DefaultMinSuccessful,
      maxDiscarded = DefaultMaxDiscarded,
      minSize = DefaultMinSize,
      maxSize = DefaultMaxSize,
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
    val params = getParameter(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.minSuccessful should equal (DefaultMinSuccessful)
  }

  def maxDiscardRatio(maxDiscardedTests: Int, minSuccessfulTests: Int): Float =
    (maxDiscardedTests: Float)/(minSuccessfulTests: Float)

  // maxDiscarded
  test("getParameter returns passed maxDiscarded config param") {
    val params = getParameter(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.maxDiscardedFactor.value should equal (maxDiscardRatio(PassedMaxDiscarded + 1, params.minSuccessful))
  }

  test("getParameter throws IAE if passed multiple maxDiscarded config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MaxDiscarded(33), MaxDiscarded(34)), defaultConfig)
    }
  }

  test("getParameter returns default maxDiscarded config param if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxDiscardedFactor.value should equal (maxDiscardRatio(DefaultMaxDiscarded + 1, params.minSuccessful))
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

  // maxSize
  test("getParameter returns passed maxSize config param") {
    val params = getParameter(Seq(MaxSize(PassedMaxSize + 100)), defaultConfig)
    params.maxSize.value should equal (PassedMaxSize + 100)
  }

  test("getParameter throws IAE if passed multiple maxSize config params") {
    intercept[IllegalArgumentException] {
      getParameter(Seq(MaxSize(33), MaxSize(34)), defaultConfig)
    }
  }

  test("getParameter returns default maxSize config param if none passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxSize should equal (DefaultMaxSize)
  }

  test("getParameter returns default maxSize config param if none passed and MinSuccessful changed") {
    val params = getParameter(Seq(MinSize(PassedMinSize)), defaultConfig)
    println("Params.maxsize: " + params.maxSize)
    params.maxSize should equal (DefaultMaxSize)
    params.minSize should equal (PassedMinSize)
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
    params.maxDiscardedFactor.value should equal (maxDiscardRatio(DefaultMaxDiscarded + 1, params.minSuccessful))
    params.minSize should equal (DefaultMinSize)
    params.maxSize should equal (DefaultMaxSize)
    params.workers should equal (DefaultWorkers)
  }

  test("getParameter returns all passed if all config params passed") {
    val params = getParameter(Seq(MinSuccessful(PassedMinSuccessful), MaxDiscarded(PassedMaxDiscarded), MinSize(PassedMinSize), MaxSize(PassedMaxSize), Workers(PassedWorkers)), defaultConfig)
    params.minSuccessful should equal (PassedMinSuccessful)
    params.maxDiscardedFactor.value should equal (maxDiscardRatio(PassedMaxDiscarded + 1, params.minSuccessful))
    params.minSize should equal (PassedMinSize)
    params.maxSize should equal (PassedMaxSize)
    params.workers should equal (PassedWorkers)
  }
}
