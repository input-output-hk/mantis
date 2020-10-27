package io.iohk.ethereum.mallet.common

import akka.util.ByteString
import org.bouncycastle.util.encoders.Hex

object StringUtil {

  def unquote(s: String): String = {
    require(s.startsWith("\"") && s.endsWith("\"") && s.length > 1, s"[$s] is not quoted")
    s.substring(1, s.length - 1)
  }

  def prefix0x(s: String): String =
    if (s.startsWith("0x")) s else "0x" + s

  def drop0x(s: String): String =
    if (s.startsWith("0x")) s.substring(2) else s

  def hexToBytes(s: String): ByteString = {
    val digits = drop0x(s)
    val padded = if (digits.length % 2 == 0) digits else "0" + digits
    ByteString(Hex.decode(padded))
  }

  def hexToBigInt(s: String): BigInt = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    BigInt(1, Hex.decode(normalized))
  }
}
