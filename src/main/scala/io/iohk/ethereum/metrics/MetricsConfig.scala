package io.iohk.ethereum.metrics

import com.typesafe.config.{Config ⇒ TypesafeConfig}

final case class MetricsConfig(
  enabled: Boolean,
  host: String,
  port: Int,
  queueSize: Int,
  logErrors: Boolean,
  environment: String, // `public`, `private`
  deployment: String   // `testnet-kevm`, `testnet-iele`
)

object MetricsConfig {
  object Keys {
    final val Metrics = "metrics"

    final val Enabled = "enabled"
    final val Host = "host"
    final val Port = "port"
    final val QueueSize = "queue-size"
    final val LogErrors = "log-errors"

    final val Environment = "environment"
    final val Deployment = "deployment"
  }

  def apply(mantisConfig: TypesafeConfig): MetricsConfig = {
    val config = mantisConfig.getConfig(Keys.Metrics)

    val enabled = config.getBoolean(Keys.Enabled)
    val host = config.getString(Keys.Host)
    val port = config.getInt(Keys.Port)
    val queueSize = config.getInt(Keys.QueueSize)
    val logErrors = config.getBoolean(Keys.LogErrors)

    val environment = config.getString(Keys.Environment)
    val deployment = config.getString(Keys.Deployment)

    MetricsConfig(
      enabled = enabled,
      host = host,
      port = port,
      queueSize = queueSize,
      logErrors = logErrors,
      environment = environment,
      deployment = environment
    )
  }
}
