package io.iohk.ethereum.metrics

import com.typesafe.config.{Config => TypesafeConfig}

final case class MetricsConfig(
    clientId: String,
    enabled: Boolean,
    port: Int
)

object MetricsConfig {
  object Keys {
    final val Metrics = "metrics"

    final val Enabled = "enabled"
    final val Port = "port"
  }

  def apply(config: TypesafeConfig): MetricsConfig = {
    val clientId = config.getString("client-id")
    val metricsConfig = config.getConfig(Keys.Metrics)

    val enabled = metricsConfig.getBoolean(Keys.Enabled)
    val port = metricsConfig.getInt(Keys.Port)

    MetricsConfig(
      clientId = clientId,
      enabled = enabled,
      port = port
    )
  }
}
