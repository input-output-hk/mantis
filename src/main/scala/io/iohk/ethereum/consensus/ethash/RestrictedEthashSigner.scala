package io.iohk.ethereum.consensus.ethash

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

object RestrictedEthashSigner {

  def validateSignature(blockHeader: BlockHeader, allowedMiners: Set[ByteString]): Boolean = {
    val signature = blockHeader.extraData.takeRight(ECDSASignature.EncodedLength)
    val blockHeaderWithoutSig =
      blockHeader.copy(extraData = blockHeader.extraData.dropRight(ECDSASignature.EncodedLength))
    val encodedBlockHeader = getEncodedWithoutNonce(blockHeaderWithoutSig)
    val headerHash = crypto.kec256(encodedBlockHeader)
    val maybePubKey = for {
      sig <- ECDSASignature.fromBytes(signature)
      pubKeyBytes <- sig.publicKey(headerHash)
    } yield ByteString.fromArrayUnsafe(pubKeyBytes)

    maybePubKey.fold(false)(pubKey => allowedMiners.contains(pubKey))
  }

  def signHeader(blockHeader: BlockHeader, keyPair: AsymmetricCipherKeyPair): BlockHeader = {
    val encoded = getEncodedWithoutNonce(blockHeader)
    val hash = crypto.kec256(encoded)
    val signed = ECDSASignature.sign(hash, keyPair)
    val sigBytes = signed.toBytes
    blockHeader.copy(extraData = blockHeader.extraData ++ sigBytes)
  }

}
