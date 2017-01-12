package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.spongycastle.math.ec.ECPoint

object AuthResponseMessage {

  private val PublicKeyLength = 64
  private val NonceLength = 32
  val encodedLength = PublicKeyLength + NonceLength + 1

  def decode(input: Array[Byte]): AuthResponseMessage = {
    AuthResponseMessage(
      ephemeralPublicKey = curve.getCurve.decodePoint(Array(4.toByte) ++ input.take(PublicKeyLength)),
      nonce = ByteString(input.drop(PublicKeyLength).take(NonceLength)),
      knownPeer = input(PublicKeyLength + NonceLength) == 1)
  }
}

case class AuthResponseMessage(ephemeralPublicKey: ECPoint, nonce: ByteString, knownPeer: Boolean) {

  lazy val encoded: ByteString = ByteString(
    ephemeralPublicKey.getEncoded(false).drop(1) ++
    nonce ++
    Array(if (knownPeer) 1.toByte else 0.toByte))
}
