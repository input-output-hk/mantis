package io.iohk.ethereum.utils

import akka.util.ByteString

import scala.collection.mutable
import scala.math.Ordering.Implicits._
import scala.collection.mutable.ArrayBuilder

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

  implicit val byteStringOrdering: Ordering[ByteString] = {
    Ordering.by[ByteString, Seq[Byte]](_.toSeq)
  }

  def concatByteStrings(head: ByteStringElement, tail: ByteStringElement*): ByteString = {
    val it = head +: tail
    concatByteStrings(it)
  }

  def concatByteStrings(elements: Iterable[ByteStringElement]): ByteString = {
    val len = elements.foldLeft(0)(_ + _.len)
    val bb = java.nio.ByteBuffer.allocate(len)
    elements.foreach { el => bb.put(el.asByteArray) }
    ByteString.fromArrayUnsafe( bb.array() )
  }

}
