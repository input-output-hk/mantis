package io.iohk.ethereum

import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.params.ECPublicKeyParameters
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
}
