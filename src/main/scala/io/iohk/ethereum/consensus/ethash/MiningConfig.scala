package io.iohk.ethereum
package consensus
package ethash

import com.typesafe.config.{Config ⇒ TypesafeConfig}

import scala.concurrent.duration.{FiniteDuration, _}

final case class MiningConfig(
  ommersPoolSize: Int, // NOTE was only used to instantiate OmmersPool
  ommerPoolQueryTimeout: FiniteDuration,
  ethashDir: String,
  mineRounds: Int
)

object MiningConfig {
  def apply(mantisConfig: TypesafeConfig): MiningConfig = {
    val miningConfig = mantisConfig.getConfig("mining")

    new MiningConfig(
      ommersPoolSize = miningConfig.getInt("ommers-pool-size"),
      ommerPoolQueryTimeout = miningConfig.getDuration("ommer-pool-query-timeout").toMillis.millis,
      ethashDir = miningConfig.getString("ethash-dir"),
      mineRounds = miningConfig.getInt("mine-rounds")
    )
  }
}
