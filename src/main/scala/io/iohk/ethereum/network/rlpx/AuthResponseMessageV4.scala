package io.iohk.ethereum.network.rlpx

import akka.util.ByteString

import org.bouncycastle.math.ec.ECPoint

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPDecoder
import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList

object AuthResponseMessageV4 {

  implicit val rlpEncDec: RLPEncoder[AuthResponseMessageV4] with RLPDecoder[AuthResponseMessageV4] =
    new RLPEncoder[AuthResponseMessageV4] with RLPDecoder[AuthResponseMessageV4] {
      override def encode(obj: AuthResponseMessageV4): RLPEncodeable = {
        import obj._
        //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
        RLPList(ephemeralPublicKey.getEncoded(false).drop(1), nonce.toArray[Byte], version)
      }

      override def decode(rlp: RLPEncodeable): AuthResponseMessageV4 = rlp match {
        case RLPList(ephemeralPublicKeyBytes, nonce, version, _*) =>
          val ephemeralPublicKey =
            curve.getCurve.decodePoint(ECDSASignature.UncompressedIndicator +: (ephemeralPublicKeyBytes: Array[Byte]))
          AuthResponseMessageV4(ephemeralPublicKey, ByteString(nonce: Array[Byte]), version)
        case _ => throw new RuntimeException("Cannot decode auth response message")
      }
    }
}

case class AuthResponseMessageV4(ephemeralPublicKey: ECPoint, nonce: ByteString, version: Int)
