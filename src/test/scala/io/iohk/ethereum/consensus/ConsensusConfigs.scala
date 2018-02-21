package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.consensus.ethash.EthashConfig
import io.iohk.ethereum.domain.Address

/** Provides utility values used throughout tests */
object ConsensusConfigs {
  final val blockCacheSize = 30
  final val coinbaseAddressNum = 42
  final val coinbase = Address(coinbaseAddressNum)

  //noinspection ScalaStyle
  final val ethashConfig = new EthashConfig(
    ommersPoolSize = 30,
    ommerPoolQueryTimeout = Timeouts.normalTimeout,
    ethashDir = "~/.ethash",
    mineRounds = 100000
  )

  final val consensusConfig: ConsensusConfig = new ConsensusConfig(
    protocol = Protocol.Ethash,
    coinbase = coinbase,
    activeTimeout = Timeouts.shortTimeout,
    headerExtraData = ByteString.empty,
    blockCacheSize = blockCacheSize,
    getTransactionFromPoolTimeout = ethashConfig.ommerPoolQueryTimeout,
    miningEnabled = false
  )

  final val fullConsensusConfig = FullConsensusConfig(consensusConfig, ethashConfig)
}
