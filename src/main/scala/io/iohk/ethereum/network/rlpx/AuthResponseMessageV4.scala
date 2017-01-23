package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import org.spongycastle.math.ec.ECPoint

object AuthResponseMessageV4 {

  implicit val rlpEncDec = new RLPEncoder[AuthResponseMessageV4] with RLPDecoder[AuthResponseMessageV4] {
    override def encode(obj: AuthResponseMessageV4): RLPEncodeable = {
      import obj._
      RLPList(ephemeralPublicKey.getEncoded(false).drop(1), nonce.toArray[Byte], version)
    }

    override def decode(rlp: RLPEncodeable): AuthResponseMessageV4 = rlp match {
      case RLPList(ephemeralPublicKeyBytes, nonce, version, _*) =>
        val ephemeralPublicKey = curve.getCurve.decodePoint(Array[Byte](0x04) ++ (ephemeralPublicKeyBytes: Array[Byte]))
        AuthResponseMessageV4(ephemeralPublicKey, ByteString(nonce: Array[Byte]), version)
      case _ => throw new RuntimeException("Cannot decode auth response message")
    }
  }
}

case class AuthResponseMessageV4(ephemeralPublicKey: ECPoint, nonce: ByteString, version: Int)
