package io.iohk.ethereum.rpcTest
import org.bouncycastle.util.encoders.Hex

package object utils {

  def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  def hexToBigInt(s: String): BigInt =
    BigInt(decode(s))
}
