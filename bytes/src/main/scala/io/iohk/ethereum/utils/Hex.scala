package io.iohk.ethereum.utils

object Hex {
  def toHexString(bytes: Array[Byte]): String =
    bytes.map("%02x".format(_)).mkString

  def decode(hex: String): Array[Byte] =
    hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
}
