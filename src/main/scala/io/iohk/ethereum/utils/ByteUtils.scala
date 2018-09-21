package io.iohk.ethereum.utils

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteString

import scala.util.Random

object ByteUtils {

  def bigIntegerToBytes(b: BigInteger, numBytes: Int): Array[Byte] = {
    val bytes = new Array[Byte](numBytes)
    val biBytes = b.toByteArray
    val start = if (biBytes.length == numBytes + 1) 1 else 0
    val length = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }

  def toBigInt(bytes: ByteString): BigInt =
    bytes.foldLeft(BigInt(0)){(n, b) => (n << 8) + (b & 0xff)}

  def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    val ret = new Array[Byte](a.length)
    var i = 0
    while (i < a.length) {
      ret(i) = (a(i) ^ b(i)).toByte
      i += 1
    }
    ret
  }

  def or(arrays: Array[Byte]*): Array[Byte] = {
    require(arrays.map(_.length).distinct.length <= 1, "All the arrays should have the same length")
    require(arrays.nonEmpty, "There should be one or more arrays")

    val zeroes = Array.fill(arrays.head.length)(0.toByte)
    arrays.foldLeft[Array[Byte]](zeroes){
      case (prevOr, array) => prevOr.zip(array).map{ case (b1, b2) => (b1 | b2).toByte }
    }
  }

  def and(arrays: Array[Byte]*): Array[Byte] = {
    require(arrays.map(_.length).distinct.length <= 1, "All the arrays should have the same length")
    require(arrays.nonEmpty, "There should be one or more arrays")

    val ones = Array.fill(arrays.head.length)(0xFF.toByte)
    arrays.foldLeft[Array[Byte]](ones){
      case (prevOr, array) => prevOr.zip(array).map{ case (b1, b2) => (b1 & b2).toByte }
    }
  }

  def randomBytes(len: Int): Array[Byte] = {
    val arr = new Array[Byte](len)
    new Random().nextBytes(arr)
    arr
  }

  def bigEndianToShort(bs: Array[Byte]): Short = {
    val n = bs(0) << 8
    (n | bs(1) & 0xFF).toShort
  }

  def padLeft(bytes: ByteString, length: Int, byte: Byte = 0): ByteString = {
    val l = math.max(0, length - bytes.length)
    val fill = Seq.fill[Byte](l)(byte)
    fill ++: bytes
  }

  def compactPickledBytes(buffer: ByteBuffer): ByteString = {
    val data = Array.ofDim[Byte](buffer.limit)
    buffer.rewind()
    buffer.get(data)
    ByteString(data)
  }


  def bytesToInts(bytes: Array[Byte], bigEndian: Boolean): Array[Int] = {
    val ret = new Array[Int](bytes.length / 4)
    bytesToIntsMut(bytes, ret, bigEndian)
    ret
  }

  def intsToBytes(ints: Array[Int], bigEndian: Boolean): Array[Byte] = {
    val ret = new Array[Byte](ints.length * 4)
    intsToBytesMut(ints, ret, bigEndian)
    ret
  }

  def getIntFromWord(arr: Array[Byte]): Int = {
    ByteBuffer.wrap(arr, 0, 4).order(ByteOrder.LITTLE_ENDIAN).getInt
  }

  def intsToBytesMut(arr: Array[Int], b: Array[Byte], bigEndian: Boolean) {
    if (!bigEndian) {
      var off = 0
      var i = 0
      while (i < arr.length) {
        val ii = arr(i)
        b(off) = (ii & 0xFF).toByte
        off += 1
        b(off) = ((ii >> 8) & 0xFF).toByte
        off += 1
        b(off)= ((ii >> 16) & 0xFF).toByte
        off += 1
        b(off) = ((ii >> 24) & 0xFF).toByte
        off += 1

        i = i + 1
      }
    } else {
      var off = 0
      var i = 0
      while (i < arr.length) {
        val ii = arr(i)
        b(off) = (ii >> 24).toByte
        off += 1
        b(off) = ((ii >> 16) & 0xFF).toByte
        off += 1
        b(off)= ((ii >> 8) & 0xFF).toByte
        off += 1
        b(off) = (ii & 0xFF).toByte
        off += 1

        i = i + 1
      }
    }
  }

  def bytesToIntsMut(b: Array[Byte], arr: Array[Int], bigEndian: Boolean) {
    if (!bigEndian) {
      var off = 0
      var i = 0
      while (i < arr.length) {
        var ii: Int = b(off) & 0x000000FF
        off += 1
        ii |= (b(off) << 8) & 0x0000FF00
        off += 1
        ii |= (b(off) << 16) & 0x00FF0000
        off += 1
        ii |= (b(off) << 24)
        off += 1
        arr(i) = ii

        i = i + 1
      }
    } else {
      var off = 0
      var i = 0

      while (i < arr.length) {
        var ii: Int = b(off << 24)
        off += 1
        ii |= (b(off) << 16) & 0x0000FF00
        off += 1
        ii |= (b(off) << 8) & 0x00FF0000
        off += 1
        ii |= b(off) & 0x000000FF
        off += 1
        arr(i) = ii

        i = i + 1
      }
    }
  }

}
