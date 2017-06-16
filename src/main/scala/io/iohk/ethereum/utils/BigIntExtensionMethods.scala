package io.iohk.ethereum.utils

object BigIntExtensionMethods {
  implicit class BigIntAsUnsigned(val srcBigInteger: BigInt) extends AnyVal {
    def toUnsignedByteArray: Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray.head == 0) asByteArray.tail
      else asByteArray
    }
  }
}
