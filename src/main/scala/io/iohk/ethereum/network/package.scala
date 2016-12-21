package io.iohk.ethereum

import java.math.BigInteger

import io.iohk.ethereum.crypto._
import org.spongycastle.util.encoders.Hex

package object network {

  def publicKeyFromNodeId(nodeId: String) = {
    val bytes = Array(4.toByte) ++ // uncompressed
      Hex.decode(nodeId)
    curve.getCurve.decodePoint(bytes)
  }

  def bigIntegerToBytes(b: BigInteger, numBytes: Int): Array[Byte] = {
    val bytes = new Array[Byte](numBytes)
    val biBytes = b.toByteArray
    val start = if (biBytes.length == numBytes + 1) 1 else 0
    val length = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }


}
