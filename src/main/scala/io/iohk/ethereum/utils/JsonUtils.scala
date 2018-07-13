package io.iohk.ethereum.utils

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object JsonUtils {
  def pretty[A <: AnyRef](obj: A): String = Serialization.writePretty(obj)(DefaultFormats)
}
