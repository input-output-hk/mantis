package io.iohk.ethereum

import io.iohk.ethereum.crypto._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.encoders.Hex

import java.io.File
import java.io.PrintWriter
import java.net.Inet6Address
import java.net.InetAddress
import java.security.SecureRandom
import scala.io.Source

package object network {

  val ProtocolVersion = 4

  implicit class ECPublicKeyParametersNodeId(val pubKey: ECPublicKeyParameters) extends AnyVal {
    def toNodeId: Array[Byte] =
      pubKey
        .asInstanceOf[ECPublicKeyParameters]
        .getQ
        .getEncoded(false)
        .drop(1) // drop type info
  }

  def publicKeyFromNodeId(nodeId: String): ECPoint = {
    val bytes = ECDSASignature.UncompressedIndicator +: Hex.decode(nodeId)
    curve.getCurve.decodePoint(bytes)
  }

  def loadAsymmetricCipherKeyPair(filePath: String, secureRandom: SecureRandom): AsymmetricCipherKeyPair = {
    val file = new File(filePath)
    if (!file.exists()) {
      val keysValuePair = generateKeyPair(secureRandom)

      //Write keys to file
      val (priv, pub) = keyPairToByteArrays(keysValuePair)
      require(file.getParentFile.exists() || file.getParentFile.mkdirs(), "Key's file parent directory creation failed")
      val writer = new PrintWriter(filePath)
      try writer.write(Hex.toHexString(priv) + "\n" + Hex.toHexString(pub))
      finally writer.close()

      keysValuePair
    } else {
      val reader = Source.fromFile(filePath)
      try {
        val privHex = reader.getLines().next()
        keyPairFromPrvKey(Hex.decode(privHex))
      } finally reader.close()
    }
  }

  /** Given an address, returns the corresponding host name for the URI.
    * All IPv6 addresses are enclosed in square brackets.
    *
    * @param address, whose host name will be obtained
    * @return host name associated with the address
    */
  def getHostName(address: InetAddress): String = {
    val hostName = address.getHostAddress
    address match {
      case _: Inet6Address => s"[$hostName]"
      case _               => hostName
    }
  }

}
