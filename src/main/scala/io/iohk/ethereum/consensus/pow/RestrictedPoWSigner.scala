package io.iohk.ethereum.consensus.pow

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

object RestrictedPoWSigner {

  def validateSignature(blockHeader: BlockHeader, allowedMiners: Set[ByteString]): Boolean = {
    val signature = blockHeader.extraData.takeRight(ECDSASignature.EncodedLength)
    val headerHash = hashHeaderForSigning(blockHeader)
    val maybePubKey = ECDSASignature.fromBytes(signature).flatMap(_.publicKey(headerHash))

    maybePubKey.exists(allowedMiners.contains)
  }

  def signHeader(blockHeader: BlockHeader, keyPair: AsymmetricCipherKeyPair): BlockHeader = {
    val hash = hashHeaderForSigning(blockHeader)
    val signed = ECDSASignature.sign(hash.toArray, keyPair)
    val sigBytes = signed.toBytes
    blockHeader.withAdditionalExtraData(sigBytes)
  }

  def hashHeaderForSigning(blockHeader: BlockHeader): ByteString = {
    val blockHeaderWithoutSig =
      if (blockHeader.extraData.length >= ECDSASignature.EncodedLength)
        blockHeader.dropRightNExtraDataBytes(ECDSASignature.EncodedLength)
      else blockHeader
    val encodedBlockHeader = getEncodedWithoutNonce(blockHeaderWithoutSig)
    ByteString(crypto.kec256(encodedBlockHeader))
  }

}
