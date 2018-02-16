package io.iohk.ethereum
package consensus
package ethash

import akka.util.ByteString
import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.iohk.ethereum.validators.BlockHeaderValidatorImpl

import scala.concurrent.duration.{FiniteDuration, _}

final case class MiningConfig(
  ommersPoolSize: Int, // NOTE was only used to instantiate OmmersPool
  blockCacheSize: Int, // NOTE only used in BlockGenerator
  ommerPoolQueryTimeout: FiniteDuration,
  headerExtraData: ByteString, // only used in BlockGenerator
  ethashDir: String,
  mineRounds: Int
)

object MiningConfig {
  def apply(etcClientConfig: TypesafeConfig): MiningConfig = {
    val miningConfig = etcClientConfig.getConfig("mining")

    new MiningConfig(
      blockCacheSize = miningConfig.getInt("block-cashe-size"),
      ommersPoolSize = miningConfig.getInt("ommers-pool-size"),
      ommerPoolQueryTimeout = miningConfig.getDuration("ommer-pool-query-timeout").toMillis.millis,
      headerExtraData =
        ByteString(miningConfig
          .getString("header-extra-data").getBytes)
          .take(BlockHeaderValidatorImpl.MaxExtraDataSize),
      ethashDir = miningConfig.getString("ethash-dir"),
      mineRounds = miningConfig.getInt("mine-rounds")
    )
  }
}
