package io.iohk.ethereum.consensus.pow
package validators

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidatorSkeleton
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class MockedPowBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends BlockHeaderValidatorSkeleton(blockchainConfig) {

  protected def difficulty: DifficultyCalculator = DifficultyCalculator(blockchainConfig)

  override def validateEvenMore(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    Right(BlockHeaderValid)

}
