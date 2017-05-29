package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessageV4 extends AuthInitiateEcdsaCodec {

  implicit class AuthInitiateMessageV4Enc(obj: AuthInitiateMessageV4) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import obj._
      //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
      RLPList(encodeECDSA(signature), publicKey.getEncoded(false).drop(1), nonce, version)
    }
  }

  implicit class AuthInitiateMessageV4Dec(val bytes: Array[Byte]) extends AnyVal {
    def toAuthInitiateMessageV4: AuthInitiateMessageV4 = rawDecode(bytes) match {
      case RLPList(signatureBytes, publicKeyBytes, nonce, version, _*) =>
        val signature = decodeECDSA(signatureBytes)
        val publicKey = curve.getCurve.decodePoint(ECDSASignature.uncompressedIndicator +: (publicKeyBytes: Array[Byte]))
        AuthInitiateMessageV4(signature, publicKey, ByteString(nonce: Array[Byte]), version)
      case _ => throw new RuntimeException("Cannot decode auth initiate message")
    }
  }
}

case class AuthInitiateMessageV4(signature: ECDSASignature, publicKey: ECPoint, nonce: ByteString, version: Int)
