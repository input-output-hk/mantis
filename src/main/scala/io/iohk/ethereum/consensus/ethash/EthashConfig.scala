package io.iohk.ethereum
package consensus
package ethash

import com.typesafe.config.{Config ⇒ TypesafeConfig}

import scala.concurrent.duration.{FiniteDuration, _}

final case class EthashConfig(
  ommersPoolSize: Int, // NOTE was only used to instantiate OmmersPool
  ommerPoolQueryTimeout: FiniteDuration,
  ethashDir: String,
  mineRounds: Int
)

object EthashConfig {
  object Keys {
    final val OmmersPoolSize = "ommers-pool-size"
    final val OmmerPoolQueryTimeout = "ommer-pool-query-timeout"
    final val EthashDir = "ethash-dir"
    final val MineRounds = "mine-rounds"
  }

  def apply(mantisConfig: TypesafeConfig): EthashConfig = {
    val miningConfig = mantisConfig.getConfig(Protocol.Names.Ethash)

    new EthashConfig(
      ommersPoolSize = miningConfig.getInt(Keys.OmmersPoolSize),
      ommerPoolQueryTimeout = miningConfig.getDuration(Keys.OmmerPoolQueryTimeout).toMillis.millis,
      ethashDir = miningConfig.getString(Keys.EthashDir),
      mineRounds = miningConfig.getInt(Keys.MineRounds)
    )
  }
}
