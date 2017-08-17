package io.iohk.ethereum.utils

object NumericUtils {

  def parseHexOrDecNumber(s: String): BigInt =
    if (s.startsWith("0x"))
      BigInt(s.drop(2), 16)
    else
      BigInt(s)

  def parseHexNumber(s: String): BigInt =
    BigInt(s.replaceFirst("^0x", ""), 16)
}
