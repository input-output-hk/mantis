package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString

import io.iohk.ethereum.consensus.pow.RestrictedPoWSigner
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.RestrictedPoWHeaderExtraDataError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValidatorSkeleton
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

object RestrictedEthashBlockHeaderValidator extends BlockHeaderValidatorSkeleton {

  override protected def validateEvenMore(blockHeader: BlockHeader)(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockHeaderError, BlockHeaderValid] =
    PoWBlockHeaderValidator.validateEvenMore(blockHeader)

  val ExtraDataMaxSize: Int = BlockHeaderValidator.MaxExtraDataSize + ECDSASignature.EncodedLength

  private def validateSignatureAgainstAllowedMiners(
      blockHeader: BlockHeader,
      allowedMiners: Set[ByteString]
  ): Either[BlockHeaderError, BlockHeaderValid] = {
    val emptyOrValid = allowedMiners.isEmpty || RestrictedPoWSigner.validateSignature(blockHeader, allowedMiners)
    Either.cond(emptyOrValid, BlockHeaderValid, RestrictedPoWHeaderExtraDataError)
  }

  override protected def validateExtraData(
      blockHeader: BlockHeader
  )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = {
    val tooLargeExtraData = blockHeader.extraData.length > ExtraDataMaxSize

    if (tooLargeExtraData) {
      Left(RestrictedPoWHeaderExtraDataError)
    } else {
      validateSignatureAgainstAllowedMiners(blockHeader, blockchainConfig.allowedMinersPublicKeys)
    }
  }
}
