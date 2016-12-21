package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto.{ECIESPublicKeyEncoder, ECDSASignature}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.math.ec.ECPoint
import io.iohk.ethereum.crypto._

object AuthInitiateMessage {

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

  def encode(): ByteString = {
    signature.canonicalise().encode() ++
      ephemeralPublicHash ++
      ECIESPublicKeyEncoder.getEncoded(new ECPublicKeyParameters(publicKey, curve)).drop(1) ++
      nonce ++
      ByteString(if (knownPeer) 1.toByte else 0.toByte)
  }

}
