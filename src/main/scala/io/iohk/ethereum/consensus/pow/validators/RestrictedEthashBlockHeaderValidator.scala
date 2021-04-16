package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.RestrictedPoWSigner
import io.iohk.ethereum.consensus.validators.BlockHeaderError.RestrictedPoWHeaderExtraDataError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class RestrictedEthashBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends PoWBlockHeaderValidator(blockchainConfig) {

  val ExtraDataMaxSize = BlockHeaderValidator.MaxExtraDataSize + ECDSASignature.EncodedLength

  private def validateSignatureAgainstAllowedMiners(
      blockHeader: BlockHeader,
      allowedMiners: Set[ByteString]
  ): Either[BlockHeaderError, BlockHeaderValid] = {
    val emptyOrValid = allowedMiners.isEmpty || RestrictedPoWSigner.validateSignature(blockHeader, allowedMiners)
    Either.cond(emptyOrValid, BlockHeaderValid, RestrictedPoWHeaderExtraDataError)
  }

  override protected def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val tooLargeExtraData = blockHeader.extraData.length > ExtraDataMaxSize

    if (tooLargeExtraData) {
      Left(RestrictedPoWHeaderExtraDataError)
    } else {
      validateSignatureAgainstAllowedMiners(blockHeader, blockchainConfig.allowedMinersPublicKeys)
    }
  }
}
