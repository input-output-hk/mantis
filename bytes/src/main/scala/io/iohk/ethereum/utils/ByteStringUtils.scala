package io.iohk.ethereum.utils

import akka.util.ByteString

object ByteStringUtils {
  def hash2string(hash: ByteString): String =
    Hex.toHexString(hash.toArray[Byte])

  // unsafe
  def string2hash(hash: String): ByteString =
    ByteString(Hex.decode(hash))


  implicit class Padding(val bs: ByteString) extends AnyVal {
    def padToByteString(length: Int, b: Byte): ByteString = {
      if (length <= bs.length) bs else {
        val len = Math.max(bs.length, length)
        val result = new Array[Byte](len)
        bs.copyToArray(result, 0)
        var i = bs.length
        while (i < len) {
          result.update(i, b)
          i += 1
        }
        ByteString.fromArray(result)
      }
    }
  }


  sealed trait ByteStringElement {
    def len: Int
    def asByteArray: Array[Byte]
  }

  implicit class ByteStringSelfElement(val bs: ByteString) extends ByteStringElement {
    def len: Int = bs.length
    def asByteArray: Array[Byte] = bs.toArray
  }

  implicit class ByteStringArrayElement(val ar: Array[Byte]) extends ByteStringElement {
    def len: Int = ar.length
    def asByteArray: Array[Byte] = ar
  }

  implicit class ByteStringByteElement(val b: Byte) extends ByteStringElement {
    def len: Int = 1
    def asByteArray: Array[Byte] = Array(b)
  }

  def concatByteStrings(bse: ByteStringElement*): ByteString = {
    val totalLength = bse.map(_.len).sum
    val result = new Array[Byte](totalLength)
    bse.foldLeft(0)( (i, el) => {
      val arr = el.asByteArray
      System.arraycopy(arr, 0, result, i, el.len)
      i + el.len
    })
    ByteString.fromArray(result)
  }

}
