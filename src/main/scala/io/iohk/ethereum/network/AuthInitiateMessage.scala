package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessage {

  val encodedLength = ECDSASignature.encodedLength+32+64+32+1

  def decode(input: Array[Byte]) = {
    AuthInitiateMessage(
      signature = ECDSASignature.decode(input.take(65)),
      ephemeralPublicHash = ByteString(input.slice(65, 65 + 32)),
      publicKey = curve.getCurve.decodePoint(Array(4.toByte) ++ input.slice(97, 97 + 64)),
      nonce = ByteString(input.slice(161, 161 + 32)),
      knownPeer = input(193) == 1)
  }
}

case class AuthInitiateMessage(
    signature: ECDSASignature,
    ephemeralPublicHash: ByteString,
    publicKey: ECPoint,
    nonce: ByteString,
    knownPeer: Boolean) {

  lazy val encoded: ByteString = {
    signature.encoded ++
    ephemeralPublicHash ++
    publicKey.getEncoded(false).drop(1) ++
    nonce ++
    ByteString(if (knownPeer) 1.toByte else 0.toByte)
  }

}
