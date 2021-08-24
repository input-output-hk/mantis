package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.Agharta
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.Atlantis
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.BeforeAtlantis
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.EtcFork
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.Magneto
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.Phoenix
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.BeforeByzantium
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.Berlin
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.Byzantium
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.Constantinople
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.Istanbul
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.Petersburg

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
    magnetoBlockNumber: BigInt,
    berlinBlockNumber: BigInt,
    chainId: Byte
) {
  def etcForkForBlockNumber(blockNumber: BigInt): EtcFork = blockNumber match {
    case _ if blockNumber < atlantisBlockNumber => BeforeAtlantis
    case _ if blockNumber < aghartaBlockNumber  => Atlantis
    case _ if blockNumber < phoenixBlockNumber  => Agharta
    case _ if blockNumber < magnetoBlockNumber  => Phoenix
    case _ if blockNumber >= magnetoBlockNumber => Magneto
  }

  def ethForkForBlockNumber(blockNumber: BigInt): BlockchainConfigForEvm.EthForks.Value = blockNumber match {
    case _ if blockNumber < byzantiumBlockNumber      => BeforeByzantium
    case _ if blockNumber < constantinopleBlockNumber => Byzantium
    case _ if blockNumber < petersburgBlockNumber     => Constantinople
    case _ if blockNumber < istanbulBlockNumber       => Petersburg
    case _ if blockNumber < berlinBlockNumber         => Istanbul
    case _ if blockNumber >= berlinBlockNumber        => Berlin
  }
}

object BlockchainConfigForEvm {

  object EtcForks extends Enumeration {
    type EtcFork = Value
    val BeforeAtlantis, Atlantis, Agharta, Phoenix, Magneto = Value
  }

  object EthForks extends Enumeration {
    type EthFork = Value
    val BeforeByzantium, Byzantium, Constantinople, Petersburg, Istanbul, Berlin = Value
  }

  def isEip2929Enabled(etcFork: EtcFork, ethFork: BlockchainConfigForEvm.EthForks.Value): Boolean =
    etcFork >= EtcForks.Magneto || ethFork >= EthForks.Berlin

  def apply(blockchainConfig: BlockchainConfig): BlockchainConfigForEvm = {
    import blockchainConfig._
    BlockchainConfigForEvm(
      frontierBlockNumber = forkBlockNumbers.frontierBlockNumber,
      homesteadBlockNumber = forkBlockNumbers.homesteadBlockNumber,
      eip150BlockNumber = forkBlockNumbers.eip150BlockNumber,
      eip160BlockNumber = forkBlockNumbers.eip160BlockNumber,
      eip161BlockNumber = forkBlockNumbers.eip161BlockNumber,
      byzantiumBlockNumber = forkBlockNumbers.byzantiumBlockNumber,
      constantinopleBlockNumber = forkBlockNumbers.constantinopleBlockNumber,
      istanbulBlockNumber = forkBlockNumbers.istanbulBlockNumber,
      maxCodeSize = maxCodeSize,
      accountStartNonce = accountStartNonce,
      atlantisBlockNumber = forkBlockNumbers.atlantisBlockNumber,
      aghartaBlockNumber = forkBlockNumbers.aghartaBlockNumber,
      petersburgBlockNumber = forkBlockNumbers.petersburgBlockNumber,
      phoenixBlockNumber = forkBlockNumbers.phoenixBlockNumber,
      magnetoBlockNumber = forkBlockNumbers.magnetoBlockNumber,
      berlinBlockNumber = forkBlockNumbers.berlinBlockNumber,
      chainId = chainId
    )
  }

}
