package io.iohk.ethereum

import io.iohk.ethereum.crypto._
import org.spongycastle.util.encoders.Hex

package object network {

  def publicKeyFromNodeId(nodeId: String) = {
    val bytes = Array(4.toByte) ++ // uncompressed
      Hex.decode(nodeId)
    curve.getCurve.decodePoint(bytes)
  }

}
