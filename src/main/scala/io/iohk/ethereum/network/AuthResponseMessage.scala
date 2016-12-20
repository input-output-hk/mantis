package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECIESPublicKeyEncoder
import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.math.ec.ECPoint

object AuthResponseMessage {

  def decode(input: Array[Byte]): AuthResponseMessage = {
    AuthResponseMessage(
      ephemeralPublicKey = curve.getCurve.decodePoint(Array(4.toByte) ++ input.take(64)),
      nonce = ByteString(input.drop(64).take(32)),
      knownPeer = input(96) == 1)
  }

}

case class AuthResponseMessage(ephemeralPublicKey: ECPoint, nonce: ByteString, knownPeer: Boolean) {

  def encode(): Array[Byte] = {
    ECIESPublicKeyEncoder.getEncoded(ephemeralPublicKey.asInstanceOf[ECPublicKeyParameters]) ++
      nonce ++
      Array(if (knownPeer) 1.toByte else 0.toByte)
  }

}
