package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.BlockchainConfig

/**
  * A subset of [[io.iohk.ethereum.utils.BlockchainConfig]] that is required for instantiating an [[EvmConfig]]
  * Note that `accountStartNonce` is required for a [[WorldStateProxy]] implementation that is used
  * by a given VM
  */
case class BlockchainConfigForEvm(
  frontierBlockNumber: BigInt,
  homesteadBlockNumber: BigInt,
  eip150BlockNumber: BigInt,
  eip160BlockNumber: BigInt,
  eip161BlockNumber: BigInt,
  byzantiumBlockNumber: BigInt,
  constantinopleBlockNumber: BigInt,
  maxCodeSize: Option[BigInt],
  accountStartNonce: UInt256,
  atlantisBlockNumber: BigInt,
  aghartaBlockNumber: BigInt,
  petersburgBlockNumber: BigInt,
  phoenixBlockNumber: BigInt,
  chainId: Byte
)

object BlockchainConfigForEvm {
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
