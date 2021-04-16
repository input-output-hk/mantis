package io.iohk.ethereum.consensus.pow.validators

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class PoWBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends BlockHeaderValidatorSkeleton(blockchainConfig) {

  /**
    * The difficulty calculator. This is specific to the consensus protocol.
    */
  override protected def difficulty: DifficultyCalculator = DifficultyCalculator(blockchainConfig)

  private val ethashBlockHeaderValidator = new EthashBlockHeaderValidator(blockchainConfig)

  /**
    * A hook where even more consensus-specific validation can take place.
    * For example, PoW validation is done here.
    */
  override protected[validators] def validateEvenMore(
      blockHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] =
    if (isKeccak(blockHeader.number)) KeccakBlockHeaderValidator.validateHeader(blockHeader)
    else ethashBlockHeaderValidator.validateHeader(blockHeader)

  private def isKeccak(currentBlockNumber: BigInt): Boolean =
    blockchainConfig.ecip1049BlockNumber match {
      case Some(keccakBlock) => currentBlockNumber >= keccakBlock
      case None => false
    }
}
