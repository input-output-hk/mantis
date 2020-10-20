package io.iohk.ethereum.consensus.ethash
package validators

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton}
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.utils.BlockchainConfig

class MockedPowBlockHeaderValidator(blockchainConfig: BlockchainConfig, blockchain: Blockchain)
    extends BlockHeaderValidatorSkeleton(blockchainConfig) {

  protected def difficulty: DifficultyCalculator =
    new EthashDifficultyCalculator(
      blockchainConfig,
      EthashDifficultyCalculator.grandparentsDataGetterFromBlockchain(blockchain)
    )

  def validateEvenMore(
      blockHeader: BlockHeader,
      parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] =
    Right(BlockHeaderValid)

}
