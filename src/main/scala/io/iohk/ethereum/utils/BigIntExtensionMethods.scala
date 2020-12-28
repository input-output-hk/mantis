package io.iohk.ethereum.utils

import io.iohk.ethereum.domain.UInt256

object BigIntExtensionMethods {
  implicit class BigIntAsUnsigned(val srcBigInteger: BigInt) extends AnyVal {
    def toUnsignedByteArray: Array[Byte] =
      ByteUtils.bigIntToUnsignedByteArray(srcBigInteger)

    def u256: UInt256 = UInt256(srcBigInteger)
  }
}
