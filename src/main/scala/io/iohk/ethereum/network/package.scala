package io.iohk.ethereum

import java.io.{File, PrintWriter}
import java.math.BigInteger

import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

import scala.io.Source

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

  object AsymmetricCipherKeyPairSerializable {

    def toBytes(keysValuePair: AsymmetricCipherKeyPair): (String, String) = {
      val pubKey = keysValuePair.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
      val privKey = keysValuePair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD.toByteArray
      (Hex.toHexString(pubKey), Hex.toHexString(privKey))
    }

    def fromBytes(publicKey: String, privateKey: String): AsymmetricCipherKeyPair = {
      val pub = new ECPublicKeyParameters(publicKeyFromNodeId(publicKey), curve)
      val priv = new ECPrivateKeyParameters(new BigInteger(Hex.decode(privateKey)), curve)
      new AsymmetricCipherKeyPair(pub, priv)
    }

  }

  def loadAsymmetricCipherKeyPair(filePath: String): AsymmetricCipherKeyPair = {
    if(!new File(filePath).exists()){
      val keysValuePair = generateKeyPair()

      //Write keys to file
      val (pub, priv) = AsymmetricCipherKeyPairSerializable.toBytes(keysValuePair)
      val file = new PrintWriter(filePath)
      file.write(pub ++ "\n" ++ priv)
      file.close()

      keysValuePair
    } else {
      val List(pub, priv) = Source.fromFile(filePath).getLines().toList
      AsymmetricCipherKeyPairSerializable.fromBytes(pub, priv)
    }
  }

}
