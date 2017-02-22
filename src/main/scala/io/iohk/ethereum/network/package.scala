package io.iohk.ethereum

import java.math.BigInteger

import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

package object network {

  implicit class ECPublicKeyParametersNodeId(val pubKey: ECPublicKeyParameters) extends AnyVal {
    def toNodeId: Array[Byte] =
      pubKey.asInstanceOf[ECPublicKeyParameters].getQ
      .getEncoded(false)
      .drop(1) // drop type info
  }

  def publicKeyFromNodeId(nodeId: String): ECPoint = {
    val bytes = Array(4.toByte) ++ // uncompressed
      Hex.decode(nodeId)
    curve.getCurve.decodePoint(bytes)
  }

  def getEncoded(keysValuePair: AsymmetricCipherKeyPair): (String, String) = {
    val pubKey = keysValuePair.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
    val privKey = keysValuePair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD.toByteArray
    (Hex.toHexString(pubKey), Hex.toHexString(privKey))
  }

  def fromEncoded(publicKey: String, privateKey: String): AsymmetricCipherKeyPair = {
    new AsymmetricCipherKeyPair(
      new ECPublicKeyParameters(publicKeyFromNodeId(publicKey), curve),
      new ECPrivateKeyParameters(new BigInteger(Hex.decode(privateKey)), curve))
  }
}
