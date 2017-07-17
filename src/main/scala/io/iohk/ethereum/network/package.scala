package io.iohk.ethereum

import java.io.{File, PrintWriter}
import java.security.SecureRandom

import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

import scala.io.Source

package object network {

  val ProtocolVersion = 4

  implicit class ECPublicKeyParametersNodeId(val pubKey: ECPublicKeyParameters) extends AnyVal {
    def toNodeId: Array[Byte] =
      pubKey.asInstanceOf[ECPublicKeyParameters].getQ
      .getEncoded(false)
      .drop(1) // drop type info
  }

  def publicKeyFromNodeId(nodeId: String): ECPoint = {
    val bytes = ECDSASignature.uncompressedIndicator +: Hex.decode(nodeId)
    curve.getCurve.decodePoint(bytes)
  }

  def loadAsymmetricCipherKeyPair(filePath: String, secureRandom: SecureRandom): AsymmetricCipherKeyPair = {
    val file = new File(filePath)
    if(!file.exists()){
      val keysValuePair = generateKeyPair(secureRandom)

      //Write keys to file
      val (_, priv) = keyPairToByteArrays(keysValuePair)
      file.getParentFile.mkdirs()
      val writer = new PrintWriter(filePath)
      try {
        writer.write(Hex.toHexString(priv))
      } finally {
        writer.close()
      }

      keysValuePair
    } else {
      val reader = Source.fromFile(filePath)
      try {
        val privHex = reader.mkString
        keyPairFromPrvKey(Hex.decode(privHex))
      } finally {
        reader.close()
      }
    }
  }

}
