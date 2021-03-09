package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.{Agharta, Atlantis, BeforeAtlantis, EtcFork, Phoenix}
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.{BeforeByzantium, Byzantium, Constantinople, Istanbul, Petersburg}

/** A subset of [[io.iohk.ethereum.utils.BlockchainConfig]] that is required for instantiating an [[EvmConfig]]
  * Note that `accountStartNonce` is required for a [[WorldStateProxy]] implementation that is used
  * by a given VM
  */
// FIXME manage etc/eth forks in a more sophisticated way [ETCM-249]
case class BlockchainConfigForEvm(
    // ETH forks
    frontierBlockNumber: BigInt,
    homesteadBlockNumber: BigInt,
    eip150BlockNumber: BigInt,
    eip160BlockNumber: BigInt,
    eip161BlockNumber: BigInt,
    byzantiumBlockNumber: BigInt,
    constantinopleBlockNumber: BigInt,
    istanbulBlockNumber: BigInt,
    maxCodeSize: Option[BigInt],
    accountStartNonce: UInt256,
    // ETC forks
    atlantisBlockNumber: BigInt,
    aghartaBlockNumber: BigInt,
    petersburgBlockNumber: BigInt,
    phoenixBlockNumber: BigInt,
    chainId: Byte
) {
  def etcForkForBlockNumber(blockNumber: BigInt): EtcFork = blockNumber match {
    case _ if blockNumber < atlantisBlockNumber => BeforeAtlantis
    case _ if blockNumber < aghartaBlockNumber  => Atlantis
    case _ if blockNumber < phoenixBlockNumber  => Agharta
    case _ if blockNumber >= phoenixBlockNumber => Phoenix
  }

  def ethForkForBlockNumber(blockNumber: BigInt): BlockchainConfigForEvm.EthForks.Value = blockNumber match {
    case _ if blockNumber < byzantiumBlockNumber      => BeforeByzantium
    case _ if blockNumber < constantinopleBlockNumber => Byzantium
    case _ if blockNumber < petersburgBlockNumber     => Constantinople
    case _ if blockNumber < istanbulBlockNumber       => Petersburg
    case _ if blockNumber >= istanbulBlockNumber      => Istanbul
  }
}

object BlockchainConfigForEvm {

  object EtcForks extends Enumeration {
    type EtcFork = Value
    val BeforeAtlantis, Atlantis, Agharta, Phoenix = Value
  }

  object EthForks extends Enumeration {
    type EthFork = Value
    val BeforeByzantium, Byzantium, Constantinople, Petersburg, Istanbul = Value
  }

  def apply(blockchainConfig: BlockchainConfig): BlockchainConfigForEvm = {
    import blockchainConfig._
    BlockchainConfigForEvm(
      frontierBlockNumber = frontierBlockNumber,
      homesteadBlockNumber = homesteadBlockNumber,
      eip150BlockNumber = eip150BlockNumber,
      eip160BlockNumber = eip160BlockNumber,
      eip161BlockNumber = eip161BlockNumber,
      byzantiumBlockNumber = byzantiumBlockNumber,
      constantinopleBlockNumber = constantinopleBlockNumber,
      istanbulBlockNumber = istanbulBlockNumber,
      maxCodeSize = maxCodeSize,
      accountStartNonce = accountStartNonce,
      atlantisBlockNumber = atlantisBlockNumber,
      aghartaBlockNumber = aghartaBlockNumber,
      petersburgBlockNumber = petersburgBlockNumber,
      phoenixBlockNumber = phoenixBlockNumber,
      chainId = chainId
    )
  }

}
