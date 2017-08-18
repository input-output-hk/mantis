package io.iohk.ethereum.utils

import io.iohk.ethereum.vm.UInt256

object BigIntExtensionMethods {
  implicit class BigIntAsUnsigned(val srcBigInteger: BigInt) extends AnyVal {
    def toUnsignedByteArray: Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray.head == 0) asByteArray.tail
      else asByteArray
    }

    def u256: UInt256 = UInt256(srcBigInteger)
  }
}
