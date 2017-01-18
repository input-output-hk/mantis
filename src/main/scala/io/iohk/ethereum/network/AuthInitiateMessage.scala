package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.spongycastle.math.ec.ECPoint

object AuthInitiateMessage {
  val NonceLength = 32
  val EphemeralHashLength = 32
  val PublicKeyLength = 64
  val KnownPeerLength = 1
  val EncodedLength = ECDSASignature.EncodedLength + EphemeralHashLength + PublicKeyLength + NonceLength + KnownPeerLength

  def decode(input: Array[Byte]): AuthInitiateMessage = {
    val publicKeyIndex = ECDSASignature.EncodedLength + EphemeralHashLength
    val nonceIndex = publicKeyIndex + PublicKeyLength
    val knownPeerIndex = nonceIndex + NonceLength

    AuthInitiateMessage(
      signature = ECDSASignature.decode(input.take(ECDSASignature.EncodedLength)),
      ephemeralPublicHash = ByteString(input.slice(ECDSASignature.EncodedLength, publicKeyIndex)),
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
