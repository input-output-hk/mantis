package io.iohk.ethereum.utils

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteString

import scala.util.Random

object ByteUtils {

  /**
    * Calculates number of matching bytes from the beginning of both arrays.
    * Due to performance reasons needs to be as fast as possible which means usage of while loops and var's.
    *
    * @param a - first array of bytes to check
    * @param b - second array to bytes to check
    * @return Length of common prefix shared by both arrays
    */
  def matchingLength(a: Array[Byte], b: Array[Byte]): Int = {
    var prefixLen = 0
    while (prefixLen < a.length && prefixLen < b.length && a(prefixLen) == b(prefixLen)) {
      prefixLen = prefixLen + 1
    }
    prefixLen
  }

  def bigIntegerToBytes(b: BigInteger, numBytes: Int): Array[Byte] = {
    val bytes = new Array[Byte](numBytes)
    val biBytes = b.toByteArray
    val start = if (biBytes.length == numBytes + 1) 1 else 0
    val length = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }

  def bigIntToBytes(b: BigInt, numBytes: Int): Array[Byte] =
    bigIntegerToBytes(b.bigInteger, numBytes)

  def toBigInt(bytes: ByteString): BigInt =
    bytes.foldLeft(BigInt(0)) { (n, b) => (n << 8) + (b & 0xff) }

  def bigIntToUnsignedByteArray(i: BigInt): Array[Byte] = {
    val asByteArray = i.toByteArray
    if (asByteArray.head == 0) asByteArray.tail
    else asByteArray
  }

  /**
    * Calculates xor distance between two byte arrays. Due to performance reasons needs to be as fast as possible
    * which means usage of while loops and var's.
    *
    * @param a - array of bytes to xor
    * @param b - array of bytes to xor
    * @return Array[Byte] - each element of array is equal to `(a(i) ^ b(i))`
    */
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
    arrays.foldLeft[Array[Byte]](zeroes) { case (prevOr, array) =>
      prevOr.zip(array).map { case (b1, b2) => (b1 | b2).toByte }
    }
  }

  def and(arrays: Array[Byte]*): Array[Byte] = {
    require(arrays.map(_.length).distinct.length <= 1, "All the arrays should have the same length")
    require(arrays.nonEmpty, "There should be one or more arrays")

    val ones = Array.fill(arrays.head.length)(0xff.toByte)
    arrays.foldLeft[Array[Byte]](ones) { case (prevOr, array) =>
      prevOr.zip(array).map { case (b1, b2) => (b1 & b2).toByte }
    }
  }

  def randomBytes(len: Int): Array[Byte] = {
    val arr = new Array[Byte](len)
    new Random().nextBytes(arr)
    arr
  }

  def bigEndianToShort(bs: Array[Byte]): Short = {
    val n = bs(0) << 8
    (n | bs(1) & 0xff).toShort
  }

  def padLeft(bytes: ByteString, length: Int, byte: Byte = 0): ByteString = {
    val l = math.max(0, length - bytes.length)
    val fill = Array.fill[Byte](l)(byte)
    ByteString.apply(fill) ++ bytes
  }

  def compactPickledBytesToArray(buffer: ByteBuffer): Array[Byte] = {
    val data = Array.ofDim[Byte](buffer.limit())
    buffer.rewind()
    buffer.get(data)
    data
  }

  def compactPickledBytes(buffer: ByteBuffer): ByteString = {
    ByteString(compactPickledBytesToArray(buffer))
  }

  def byteSequenceToBuffer(bytes: IndexedSeq[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes.toArray)

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

  /**
    * Converts array of Int to corresponding array of bytes. Due to performance reasons needs to be as fast as possible
    * which means usage of while loops and var's.
    *
    * @param arr - array of int's to convert
    * @param b - array for resulting byte conversion. It will be mutated in place, and it's length needs to be equal to
    *              `(arr.length * 4)`
    * @param bigEndian - param specifying which int representation should be used.
    * @return Unit
    */
  def intsToBytesMut(arr: Array[Int], b: Array[Byte], bigEndian: Boolean): Unit = {
    if (!bigEndian) {
      var off = 0
      var i = 0
      while (i < arr.length) {
        val ii = arr(i)
        b(off) = (ii & 0xff).toByte
        off += 1
        b(off) = ((ii >> 8) & 0xff).toByte
        off += 1
        b(off) = ((ii >> 16) & 0xff).toByte
        off += 1
        b(off) = ((ii >> 24) & 0xff).toByte
        off += 1

        i = i + 1
      }
    } else {
      var off = 0
      var i = 0
      while (i < arr.length) {
        val ii = arr(i)
        b(off) = ((ii >> 24) & 0xff).toByte
        off += 1
        b(off) = ((ii >> 16) & 0xff).toByte
        off += 1
        b(off) = ((ii >> 8) & 0xff).toByte
        off += 1
        b(off) = (ii & 0xff).toByte
        off += 1

        i = i + 1
      }
    }
  }

  /**
    * Converts array of bytes to corresponding array of ints. Due to performance reasons needs to be as fast as possible
    * which means usage of while loops and var's.
    *
    * @param b - array of bytes to convert
    * @param arr - array for resulting int conversion. It will be mutated in place, and it's length needs to be equal to
    *              `(b.length / 4)`
    * @param bigEndian - param specifying which int representation should be used.
    * @return Unit
    */
  def bytesToIntsMut(b: Array[Byte], arr: Array[Int], bigEndian: Boolean): Unit = {
    if (!bigEndian) {
      var off = 0
      var i = 0
      while (i < arr.length) {
        var ii: Int = b(off) & 0x000000ff
        off += 1
        ii |= (b(off) << 8) & 0x0000ff00
        off += 1
        ii |= (b(off) << 16) & 0x00ff0000
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
        var ii: Int = b(off) << 24
        off += 1
        ii |= (b(off) << 16) & 0x00ff0000
        off += 1
        ii |= (b(off) << 8) & 0x0000ff00
        off += 1
        ii |= b(off) & 0x000000ff
        off += 1
        arr(i) = ii

        i = i + 1
      }
    }
  }

}
