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

object BlockHeaderValidator {
  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val MaxGasLimit: Long = Long.MaxValue // max gasLimit is equal 2^63-1 according to EIP106
}

sealed trait BlockHeaderError

object BlockHeaderError {
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object DaoHeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError

  sealed trait HeaderGasLimitError extends BlockHeaderError
  case class HeaderGasLimitErrorConst(constant: BigInt, gasLimit: BigInt) extends HeaderGasLimitError

  // We use an Option to signify if the value was part of the validation calculation,
  // So Some(minLimit) means that io.iohk.ethereum.consensus.validators.BlockHeaderValidator#MinGasLimit
  // was used in the calculation.
  case class HeaderGasLimitErrorBounds(
    minLimit: Option[BigInt],
    gasLimit: BigInt,
    maxLimit: Option[BigInt],
    eip106BlockNumber: Option[BigInt],
    gasLimitDiff: Option[BigInt],
    gasLimitDiffLimit: Option[BigInt]
  ) extends HeaderGasLimitError

  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
}

sealed trait BlockHeaderValid
case object BlockHeaderValid extends BlockHeaderValid
