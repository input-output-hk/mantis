package io.iohk.ethereum.faucet

import com.typesafe.config.Config

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

case class SupervisorConfig(
    minBackoff: FiniteDuration,
    maxBackoff: FiniteDuration,
    randomFactor: Double,
    autoReset: FiniteDuration,
    attempts: Int,
    delay: FiniteDuration
)
object SupervisorConfig {
  def apply(typesafeConfig: Config): SupervisorConfig = {
    val supervisorConfig = typesafeConfig.getConfig("supervisor")

    SupervisorConfig(
      supervisorConfig.getDuration("min-backoff").toMillis.millis,
      supervisorConfig.getDuration("max-backoff").toMillis.millis,
      supervisorConfig.getDouble("random-factor"),
      supervisorConfig.getDuration("auto-reset").toMillis.millis,
      supervisorConfig.getInt("attempts"),
      supervisorConfig.getDuration("delay").toMillis.millis
    )

  }
}
