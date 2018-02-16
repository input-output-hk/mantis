package io.iohk.ethereum
package consensus
package ethash

import akka.util.ByteString
import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.iohk.ethereum.validators.BlockHeaderValidatorImpl

import scala.concurrent.duration.{FiniteDuration, _}

trait MiningConfig {
  val ommersPoolSize: Int // NOTE was only used to instantiate OmmersPool
  val blockCacheSize: Int // NOTE only used in BlockGenerator
  val ommerPoolQueryTimeout: FiniteDuration
  val headerExtraData: ByteString // only used in BlockGenerator
  val ethashDir: String
  val mineRounds: Int
}

object MiningConfig {
  def apply(etcClientConfig: TypesafeConfig): MiningConfig = {
    val miningConfig = etcClientConfig.getConfig("mining")

    new MiningConfig {
      val blockCacheSize: Int = miningConfig.getInt("block-cashe-size")
      val ommersPoolSize: Int = miningConfig.getInt("ommers-pool-size")
      val ommerPoolQueryTimeout: FiniteDuration = miningConfig.getDuration("ommer-pool-query-timeout").toMillis.millis
      override val headerExtraData: ByteString =
        ByteString(miningConfig
          .getString("header-extra-data").getBytes)
          .take(BlockHeaderValidatorImpl.MaxExtraDataSize)
      override val ethashDir = miningConfig.getString("ethash-dir")
      override val mineRounds = miningConfig.getInt("mine-rounds")
    }
  }
}
