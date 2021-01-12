package io.iohk.ethereum.utils

object Hex {
  def toHexString(bytes: Array[Byte]): String =
    bytes.map("%02x".format(_)).mkString

  def decode(hex: String): Array[Byte] =
    hex.toSeq.sliding(2, 2).toArray.map(s=> Integer.parseInt(s.unwrap, 16).toByte)
}
