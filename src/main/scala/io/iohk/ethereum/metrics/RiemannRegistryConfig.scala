package io.iohk.ethereum.metrics

import java.time.Duration

import io.micrometer.core.instrument.step.StepRegistryConfig

class RiemannRegistryConfig(val hostForEvents: String) extends StepRegistryConfig {

  def prefix(): String = Metrics.Prefix

  def get(key: String): String = null

  // TODO Make configurable
  // Send every five seconds
  override def step(): Duration = Duration.ofSeconds(5)

  override def enabled(): Boolean = true
}
