package io.iohk.ethereum.consensus
package validators

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields

/**
  * Validates a [[io.iohk.ethereum.domain.BlockHeader BlockHeader]].
  */
trait BlockHeaderValidator {
  def validate(
      blockHeader: BlockHeader,
      getBlockHeaderByHash: GetBlockHeaderByHash
  ): Either[BlockHeaderError, BlockHeaderValid]

  def validateHeaderOnly(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid]
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
  case object RestrictedEthashHeaderExtraDataError extends BlockHeaderError
  case object DaoHeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
  case class HeaderExtraFieldsError(
      extraFields: HeaderExtraFields,
      ecip1097Activated: Boolean,
      ecip1098Activated: Boolean
  ) extends BlockHeaderError
  case class HeaderWrongNumberOfCheckpointSignatures(sigCount: Int) extends BlockHeaderError
  case class HeaderInvalidCheckpointSignatures(invalidSignaturesWithPublics: Seq[(ECDSASignature, Option[String])])
      extends BlockHeaderError
  case object HeaderInvalidOrderOfCheckpointSignatures extends BlockHeaderError
  case class HeaderFieldNotEmptyError(msg: String) extends BlockHeaderError
  case class HeaderNotMatchParentError(msg: String) extends BlockHeaderError
  case object CheckpointHeaderTreasuryOptOutError extends BlockHeaderError

  case class HeaderUnexpectedError(msg: String) extends BlockHeaderError
}

sealed trait BlockHeaderValid
case object BlockHeaderValid extends BlockHeaderValid
