package io.iohk.ethereum.utils

import akka.util.ByteString
import org.scalatest.wordspec.AnyWordSpec

class ByteStringUtilsTest extends AnyWordSpec {

  "ByteStringUtilsTest" should {

    "concatByteStrings for simple bytestrings" in {
      val bs1 = ByteString("000")
      val bs2 = ByteString("111")
      val summarized: ByteString = bs1 ++ bs2

      val concatenated: ByteString = ByteStringUtils.concatByteStrings(bs1, bs2)
      println(s"Bs1: ${bs1} Bs2: ${bs2} Sum: ${summarized} Concat: ${concatenated}")
      assert(summarized == concatenated)
    }

  }
}
