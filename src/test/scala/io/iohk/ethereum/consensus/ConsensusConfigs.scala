package io.iohk.ethereum.consensus

import akka.util.ByteString

import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.consensus.pow.EthashConfig
import io.iohk.ethereum.domain.Address

/** Provides utility values used throughout tests */
object ConsensusConfigs {
  final val blockCacheSize = 30
  final val coinbaseAddressNum = 42
  final val coinbase: Address = Address(coinbaseAddressNum)

  //noinspection ScalaStyle
  final val ethashConfig = new EthashConfig(
    ommersPoolSize = 30,
    ommerPoolQueryTimeout = Timeouts.normalTimeout,
    ethashDir = "~/.ethash",
    mineRounds = 100000
  )

  final val miningConfig: MiningConfig = new MiningConfig(
    protocol = Protocol.PoW,
    coinbase = coinbase,
    headerExtraData = ByteString.empty,
    blockCacheSize = blockCacheSize,
    miningEnabled = false
  )

  final val fullConsensusConfig: FullConsensusConfig[EthashConfig] = FullConsensusConfig(miningConfig, ethashConfig)
}
