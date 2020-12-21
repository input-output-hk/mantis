package io.iohk.ethereum.utils

object StringUtils {

  def drop0x(s: String): String =
    if (s.startsWith("0x")) s.substring(2) else s

}
