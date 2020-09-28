package io.iohk.ethereum.utils

import akka.util.ByteString
import org.bouncycastle.util.encoders.Hex
import mouse.all._

object ByteStringUtils {
  def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])

  // unsafe
  def string2hash(hash: String): ByteString = hash |> Hex.decode |> (ByteString(_))
}
