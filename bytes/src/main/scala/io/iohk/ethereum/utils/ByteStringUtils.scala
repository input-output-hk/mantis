package io.iohk.ethereum.utils

import akka.util.ByteString

object ByteStringUtils {
  def hash2string(hash: ByteString): String =
    Hex.toHexString(hash.toArray[Byte])

  // unsafe
  def string2hash(hash: String): ByteString =
    ByteString(Hex.decode(hash))
}
