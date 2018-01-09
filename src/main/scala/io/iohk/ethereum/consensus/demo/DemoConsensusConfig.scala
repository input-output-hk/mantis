package io.iohk.ethereum.consensus
package demo


import akka.util.ByteString
import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.validators.BlockHeaderValidatorImpl

import scala.concurrent.duration.{FiniteDuration, _}

case class DemoConsensusConfig(
  IAmTheLeader: Boolean,
  ommersPoolSize: Int, // NOTE was only used to instantiate OmmersPool
  blockCacheSize: Int, // NOTE only used in BlockGenerator
  coinbase: Address,
  ommerPoolQueryTimeout: FiniteDuration,
  headerExtraData: ByteString, // only used in BlockGenerator
  ethashDir: String,
  mineRounds: Int
)

object DemoConsensusConfig {
  def apply(etcClientConfig: TypesafeConfig): DemoConsensusConfig = {
    val config = etcClientConfig.getConfig("demo-consensus")

    DemoConsensusConfig(
      IAmTheLeader = config.getBoolean("I-am-the-leader"),
      ommersPoolSize = config.getInt("ommers-pool-size"),
      blockCacheSize = config.getInt("block-cashe-size"),
      coinbase = Address(config.getString("coinbase")),
      ommerPoolQueryTimeout = config.getDuration("ommer-pool-query-timeout").toMillis.millis,
      headerExtraData = ByteString(config.getString("header-extra-data").getBytes).take(BlockHeaderValidatorImpl.MaxExtraDataSize),
      ethashDir = config.getString("ethash-dir"),
      mineRounds = config.getInt("mine-rounds")
    )
  }
}
