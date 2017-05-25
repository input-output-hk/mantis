package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessageV4 extends AuthInitiateEcdsaCodec {

  implicit val rlpEncDec = new RLPEncoder[AuthInitiateMessageV4] with RLPDecoder[AuthInitiateMessageV4] {
    override def encode(obj: AuthInitiateMessageV4): RLPEncodeable = {
      import obj._
      //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
      RLPList(encodeECDSA(signature), publicKey.getEncoded(false).drop(1), nonce, version)
    }

    override def decode(rlp: RLPEncodeable): AuthInitiateMessageV4 = rlp match {
      case RLPList(signatureBytes, publicKeyBytes, nonce, version, _*) =>
        val signature = decodeECDSA(signatureBytes)
        val publicKey = curve.getCurve.decodePoint(ECDSASignature.uncompressedIndicator +: (publicKeyBytes: Array[Byte]))
        AuthInitiateMessageV4(signature, publicKey, ByteString(nonce: Array[Byte]), version)
      case _ => throw new RuntimeException("Cannot decode auth initiate message")
    }
  }
}

case class AuthInitiateMessageV4(signature: ECDSASignature, publicKey: ECPoint, nonce: ByteString, version: Int)
