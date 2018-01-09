package io.iohk.ethereum.consensus
package validators

import io.iohk.ethereum.domain.BlockHeader

/**
 * Validates a [[io.iohk.ethereum.domain.BlockHeader BlockHeader]].
 */
trait BlockHeaderValidator {
  def validate(
    blockHeader: BlockHeader,
    getBlockHeaderByHash: GetBlockHeaderByHash
  ): Either[BlockHeaderError, BlockHeaderValid]
}

sealed trait BlockHeaderError

object BlockHeaderError {
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object DaoHeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
}

sealed trait BlockHeaderValid
case object BlockHeaderValid extends BlockHeaderValid
