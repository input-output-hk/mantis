package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessageV4 {

  implicit val rlpEncDec = new RLPEncoder[AuthInitiateMessageV4] with RLPDecoder[AuthInitiateMessageV4] {
    override def encode(obj: AuthInitiateMessageV4): RLPEncodeable = {
      import obj._
      RLPList(signature.encoded.toArray[Byte], publicKey.getEncoded(false).drop(1), nonce.toArray[Byte], version)
    }

    override def decode(rlp: RLPEncodeable): AuthInitiateMessageV4 = rlp match {
      case RLPList(signatureBytes, publicKeyBytes, nonce, version, _*) =>
        val signature = ECDSASignature.decode(signatureBytes)
        val publicKey = curve.getCurve.decodePoint(Array[Byte](0x04) ++ (publicKeyBytes: Array[Byte]))
        AuthInitiateMessageV4(signature, publicKey, ByteString(nonce: Array[Byte]), version)
      case _ => throw new RuntimeException("Cannot decode auth initiate message")
    }
  }
}

case class AuthInitiateMessageV4(signature: ECDSASignature, publicKey: ECPoint, nonce: ByteString, version: Int)
