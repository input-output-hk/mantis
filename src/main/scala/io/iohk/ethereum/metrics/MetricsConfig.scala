package io.iohk.ethereum.metrics

import com.typesafe.config.{Config ⇒ TypesafeConfig}

final case class MetricsConfig(
  enabled: Boolean,
  host: String,
  port: Int
)

object MetricsConfig {
  object Keys {
    final val Metrics = "metrics"

    final val Enabled = "enabled"
    final val Host = "host"
    final val Port = "port"
  }

  def apply(mantisConfig: TypesafeConfig): MetricsConfig = {
    val config = mantisConfig.getConfig(Keys.Metrics)

    val enabled = config.getBoolean(Keys.Enabled)
    val host = config.getString(Keys.Host)
    val port = config.getInt(Keys.Port)

    MetricsConfig(
      enabled = enabled,
      host = host,
      port = port
    )
  }
}
