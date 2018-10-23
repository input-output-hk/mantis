package io.iohk.ethereum.utils

import akka.util.ByteString
import org.bouncycastle.util.encoders.Hex

object ByteStringUtils {
  def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])
}
