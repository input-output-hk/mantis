package io.iohk.ethereum.consensus.atomixraft.validators

import io.iohk.ethereum.consensus.atomixraft.difficulty.AtomixRaftDifficulty
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class AtomixRaftBlockHeaderValidator(blockchainConfig: BlockchainConfig)
  extends BlockHeaderValidatorSkeleton(blockchainConfig) {

  protected def difficulty: DifficultyCalculator = AtomixRaftDifficulty

  protected def validateEvenMore(
    blockHeader: BlockHeader,
    parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] = Right(BlockHeaderValid) // mo more than the standard validation
}
