package org.scalatest.tools

import org.scalatest.ConfigMap

object RunnerThreadLocal {
  
  private val threadLocal: ThreadLocal[RunnerContext] = new ThreadLocal[RunnerContext]
  
  def set(configMap: ConfigMap) {
    val spanScaleFactor = configMap(Runner.SPAN_SCALE_FACTOR).asInstanceOf[Double]
    threadLocal.set(RunnerContext(spanScaleFactor))
  }
  
  def get: RunnerContext = threadLocal.get
  
}