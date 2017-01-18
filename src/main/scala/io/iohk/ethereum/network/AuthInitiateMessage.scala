package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessage {
  val encodedLength = ECDSASignature.encodedLength + 32 + 64 + 32 + 1

  def decode(input: Array[Byte]): AuthInitiateMessage = {
    val publicKeyIndex = ECDSASignature.encodedLength + 32
    val nonceIndex = publicKeyIndex + 64
    val knownPeerIndex = nonceIndex + 32

    AuthInitiateMessage(
      signature = ECDSASignature.decode(input.take(ECDSASignature.encodedLength)),
      ephemeralPublicHash = ByteString(input.slice(ECDSASignature.encodedLength, publicKeyIndex)),
      publicKey = curve.getCurve.decodePoint(Array(4.toByte) ++ input.slice(publicKeyIndex, nonceIndex)),
      nonce = ByteString(input.slice(nonceIndex, knownPeerIndex)),
      knownPeer = input(knownPeerIndex) == 1)
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
