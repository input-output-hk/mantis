package io.iohk.ethereum.metrics

import java.time.Duration

import io.micrometer.statsd.{StatsdConfig, StatsdFlavor}

// Note https://micrometer.io/docs/registry/datadog
class MantisStatsdConfig(metricsConfig: MetricsConfig) extends StatsdConfig {
  def get(key: String): String = null

  override def prefix(): String = Metrics.Prefix

  override def flavor(): StatsdFlavor = StatsdFlavor.DATADOG

  override def host(): String = metricsConfig.host

  override def port(): Int = metricsConfig.port

  override def enabled(): Boolean = metricsConfig.enabled

  override def pollingFrequency(): Duration = Duration.ofSeconds(5) // FIXME configurable
}
