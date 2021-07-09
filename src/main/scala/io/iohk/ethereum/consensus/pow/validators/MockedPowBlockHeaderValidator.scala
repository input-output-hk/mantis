package io.iohk.ethereum.consensus.pow
package validators

import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidatorSkeleton
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

object MockedPowBlockHeaderValidator extends BlockHeaderValidatorSkeleton {

  override def validateEvenMore(blockHeader: BlockHeader)(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockHeaderError, BlockHeaderValid] =
    Right(BlockHeaderValid)

}
