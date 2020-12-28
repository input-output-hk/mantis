package io.iohk.ethereum.consensus.ethash.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.ethash.RestrictedEthashSigner
import io.iohk.ethereum.consensus.validators.BlockHeaderError.RestrictedEthashHeaderExtraDataError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class RestrictedEthashBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends EthashBlockHeaderValidator(blockchainConfig) {

  val ExtraDataMaxSize = BlockHeaderValidator.MaxExtraDataSize + ECDSASignature.EncodedLength

  private def validateSignatureAgainstAllowedMiners(
      blockHeader: BlockHeader,
      allowedMiners: Set[ByteString]
  ): Either[BlockHeaderError, BlockHeaderValid] = {
    val emptyOrValid = allowedMiners.isEmpty || RestrictedEthashSigner.validateSignature(blockHeader, allowedMiners)
    Either.cond(emptyOrValid, BlockHeaderValid, RestrictedEthashHeaderExtraDataError)
  }

  override protected def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val tooLargeExtraData = blockHeader.extraData.length > ExtraDataMaxSize

    if (tooLargeExtraData) {
      Left(RestrictedEthashHeaderExtraDataError)
    } else {
      validateSignatureAgainstAllowedMiners(blockHeader, blockchainConfig.allowedMinersPublicKeys)
    }
  }
}
