package io.iohk.ethereum
package consensus
package pow

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

import com.typesafe.config.{Config => TypesafeConfig}

final case class EthashConfig(
    ommersPoolSize: Int,
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
    val miningConfig = mantisConfig.getConfig(Protocol.Names.PoW)

    val ommersPoolSize = miningConfig.getInt(Keys.OmmersPoolSize)
    val ommerPoolQueryTimeout = miningConfig.getDuration(Keys.OmmerPoolQueryTimeout).toMillis.millis
    val ethashDir = miningConfig.getString(Keys.EthashDir)
    val mineRounds = miningConfig.getInt(Keys.MineRounds)

    new EthashConfig(
      ommersPoolSize = ommersPoolSize,
      ommerPoolQueryTimeout = ommerPoolQueryTimeout,
      ethashDir = ethashDir,
      mineRounds = mineRounds
    )
  }
}
