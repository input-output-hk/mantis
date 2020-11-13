package io.iohk.ethereum.consensus.ethash.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderExtraDataError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
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
        val signature = blockHeader.extraData.takeRight(ECDSASignature.EncodedLength)
        val encodedBlockHeader = getEncodedWithoutNonce(blockHeader)
        val headerHash = crypto.kec256(encodedBlockHeader)

        (ECDSASignature.fromBytes(signature).flatMap { sig =>
          sig.publicKey(headerHash)
        } flatMap { publicKey =>
          val pubKeyAsByteSting = ByteString.fromArrayUnsafe(publicKey)
          if (blockchainConfig.allowedMinersPublicKeys.contains(pubKeyAsByteSting)) {
            Some(BlockHeaderValid)
          } else {
            None
          }
        }).toRight(HeaderExtraDataError)
      }
    }
  }
}
