package io.iohk.ethereum.consensus.ethash.validators

import io.iohk.ethereum.consensus.ethash.RestrictedEthashSigner
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderExtraDataError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class RestrictedEthashBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends EthashBlockHeaderValidator(blockchainConfig) {

  val ExtraDataMaxSize = BlockHeaderValidator.MaxExtraDataSize + ECDSASignature.EncodedLength

  override protected def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val tooLargeExtraData = blockHeader.extraData.length > ExtraDataMaxSize

    if (tooLargeExtraData) {
      Left(HeaderExtraDataError)
    } else {
      if (blockchainConfig.allowedMinersPublicKeys.isEmpty) {
        Right(BlockHeaderValid)
      } else {
        if (RestrictedEthashSigner.validateSignature(blockHeader, blockchainConfig.allowedMinersPublicKeys)) {
          Right(BlockHeaderValid)
        } else {
          Left(HeaderExtraDataError)
        }
      }
    }
  }
}
