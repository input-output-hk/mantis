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

  def bytesToHex(bytes: Array[Byte]): String = {
    val hexArray = "0123456789ABCDEF".toCharArray
    val hexChars: Array[Char] = new Array[Char](bytes.length * 2)
    var j = 0
    while ( j < bytes.length) {
      val v: Int = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
      j += 1
    }
    new String(hexChars)
  }

}
