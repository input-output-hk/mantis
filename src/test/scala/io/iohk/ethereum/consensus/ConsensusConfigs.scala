package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.consensus.ethash.MiningConfig
import io.iohk.ethereum.domain.Address

import scala.concurrent.duration.FiniteDuration

/** Provides utility values used throughout tests */
object ConsensusConfigs {
  final val coinbaseAddressNum = 42
  final val coinbase = Address(coinbaseAddressNum)

  final val miningConfig = new MiningConfig {
    override val blockCacheSize: Int = 30
    override val ommersPoolSize: Int = 30
    override val ommerPoolQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    override val headerExtraData: ByteString = ByteString.empty
    override val ethashDir: String = "~/.ethash"
    override val mineRounds: Int = 100000
  }

  final val consensusConfig: ConsensusConfig = new ConsensusConfig(
    protocol = Ethash,
    coinbase = coinbase,
    activeTimeout = Timeouts.shortTimeout,
    getTransactionFromPoolTimeout = miningConfig.ommerPoolQueryTimeout,
    miningEnabled = false
  )

  final val fullConsensusConfig = FullConsensusConfig(consensusConfig, MiningConfig)
}
