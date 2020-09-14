package io.iohk.ethereum.metrics

import io.micrometer.jmx.JmxConfig

class AppJmxConfig extends JmxConfig {
  def get(key: String): String = null

  override def prefix(): String = Metrics.MetricsPrefix

  override def domain(): String = prefix()
}
