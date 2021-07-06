package io.iohk.ethereum.consensus.pow
package validators

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidatorSkeleton
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class MockedPowBlockHeaderValidator() extends BlockHeaderValidatorSkeleton() {

  protected def difficulty: DifficultyCalculator = DifficultyCalculator

  override def validateEvenMore(blockHeader: BlockHeader)(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockHeaderError, BlockHeaderValid] =
    Right(BlockHeaderValid)

}
