package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.BlockchainConfig

/**
  * A subset of [[BlockchainConfig]] that is required for instantiating an [[EvmConfig]]
  * Note that `accountStartNonce` is required for a [[WorldStateProxy]] implementation that is used
  * by a given VM
  */
case class BlockchainConfigForEvm(
  frontierBlockNumber: BigInt,
  homesteadBlockNumber: BigInt,
  eip106BlockNumber: BigInt,
  eip150BlockNumber: BigInt,
  eip155BlockNumber: BigInt,
  eip160BlockNumber: BigInt,
  eip161BlockNumber: BigInt,
  maxCodeSize: Option[BigInt],
  accountStartNonce: UInt256
)

object BlockchainConfigForEvm {
  def apply(blockchainConfig: BlockchainConfig): BlockchainConfigForEvm = {
    import blockchainConfig._
    BlockchainConfigForEvm(
      frontierBlockNumber = frontierBlockNumber,
      homesteadBlockNumber = homesteadBlockNumber,
      eip106BlockNumber = eip106BlockNumber,
      eip150BlockNumber = eip150BlockNumber,
      eip155BlockNumber = eip155BlockNumber,
      eip160BlockNumber = eip160BlockNumber,
      eip161BlockNumber = eip161BlockNumber,
      maxCodeSize = maxCodeSize,
      accountStartNonce = accountStartNonce
    )
  }

}
