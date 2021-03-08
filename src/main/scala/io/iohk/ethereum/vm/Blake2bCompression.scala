package io.iohk.ethereum.vm

import java.util.Arrays.copyOfRange

// scalastyle:off magic.number
object Blake2bCompression {
  val MessageBytesLength = 213

  import org.bouncycastle.util.Pack

  private val IV: Array[Long] = Array(0x6a09e667f3bcc908L, 0xbb67ae8584caa73bL, 0x3c6ef372fe94f82bL,
    0xa54ff53a5f1d36f1L, 0x510e527fade682d1L, 0x9b05688c2b3e6c1fL, 0x1f83d9abfb41bd6bL, 0x5be0cd19137e2179L)

  private val PRECOMPUTED: Array[Array[Byte]] = Array(
    Array(0, 2, 4, 6, 1, 3, 5, 7, 8, 10, 12, 14, 9, 11, 13, 15),
    Array(14, 4, 9, 13, 10, 8, 15, 6, 1, 0, 11, 5, 12, 2, 7, 3),
    Array(11, 12, 5, 15, 8, 0, 2, 13, 10, 3, 7, 9, 14, 6, 1, 4),
    Array(7, 3, 13, 11, 9, 1, 12, 14, 2, 5, 4, 15, 6, 10, 0, 8),
    Array(9, 5, 2, 10, 0, 7, 4, 15, 14, 11, 6, 3, 1, 12, 8, 13),
    Array(2, 6, 0, 8, 12, 10, 11, 3, 4, 7, 15, 1, 13, 5, 14, 9),
    Array(12, 1, 14, 4, 5, 15, 13, 10, 0, 6, 9, 8, 7, 3, 2, 11),
    Array(13, 7, 12, 3, 11, 14, 1, 9, 5, 15, 8, 2, 0, 4, 6, 10),
    Array(6, 14, 11, 0, 15, 9, 3, 8, 12, 13, 1, 10, 2, 7, 4, 5),
    Array(10, 8, 7, 1, 2, 4, 6, 5, 15, 9, 3, 13, 11, 14, 12, 0)
  )

  private def bytesToInt(bytes: Array[Byte]) = Pack.bigEndianToInt(bytes, 0)

  private def bytesToLong(bytes: Array[Byte]) = Pack.littleEndianToLong(bytes, 0)

  def isValidInput(input: Array[Byte]): Boolean =
    !(input.length != MessageBytesLength || (input(212) & 0xfe) != 0)

  def parseNumberOfRounds(input: Array[Byte]): Long =
    Integer.toUnsignedLong(bytesToInt(copyOfRange(input, 0, 4)))

  /** Parses input according to the rules defined in: https://eips.ethereum.org/EIPS/eip-152
    * The encoded inputs are corresponding to the ones specified in the BLAKE2 RFC Section 3.2:
    *
    * rounds - the number of rounds - 32-bit unsigned big-endian word
    * h - the state vector - 8 unsigned 64-bit little-endian words
    * m - the message block vector - 16 unsigned 64-bit little-endian words
    * t_0, t_1 - offset counters - 2 unsigned 64-bit little-endian words
    * f - the final block indicator flag - 8-bit word
    *
    * @param input [4 bytes for rounds][64 bytes for h][128 bytes for m][8 bytes for t_0][8 bytes for t_1][1 byte for f]
    * @return all parsed inputs from input array: (rounds, h, m, t, f)
    */
  private def parseInput(input: Array[Byte]): (Long, Array[Long], Array[Long], Array[Long], Boolean) = {
    val rounds = parseNumberOfRounds(input)
    val h = new Array[Long](8)
    val m = new Array[Long](16)
    val t = new Array[Long](2)

    var i = 0
    while (i < h.length) {
      val offset = 4 + i * 8
      h(i) = bytesToLong(copyOfRange(input, offset, offset + 8))
      i += 1
    }

    var j = 0
    while (j < 16) {
      val offset = 68 + j * 8
      m(j) = bytesToLong(copyOfRange(input, offset, offset + 8))
      j += 1
    }

    t(0) = bytesToLong(copyOfRange(input, 196, 204))
    t(1) = bytesToLong(copyOfRange(input, 204, 212))
    val f = input(212) != 0
    (rounds, h, m, t, f)
  }

  def blake2bCompress(input: Array[Byte]): Option[Array[Byte]] = {
    if (isValidInput(input)) {
      val (rounds, h, m, t, f) = parseInput(input)
      compress(rounds, h, m, t, f)
      Some(convertToBytes(h))
    } else {
      None
    }
  }

  private def convertToBytes(h: Array[Long]): Array[Byte] = {
    var i = 0
    val out = new Array[Byte](h.length * 8)
    while (i < h.length) {
      System.arraycopy(Pack.longToLittleEndian(h(i)), 0, out, i * 8, 8)
      i += 1
    }
    out
  }

  private def compress(rounds: Long, h: Array[Long], m: Array[Long], t: Array[Long], f: Boolean): Unit = {
    val v = new Array[Long](16)
    val t0 = t(0)
    val t1 = t(1)
    System.arraycopy(h, 0, v, 0, 8)
    System.arraycopy(IV, 0, v, 8, 8)
    v(12) ^= t0
    v(13) ^= t1

    if (f) {
      v(14) ^= 0xffffffffffffffffL
    }

    var j = 0L
    while (j < rounds) {
      val s: Array[Byte] = PRECOMPUTED((j % 10).toInt)
      mix(v, m(s(0)), m(s(4)), 0, 4, 8, 12)
      mix(v, m(s(1)), m(s(5)), 1, 5, 9, 13)
      mix(v, m(s(2)), m(s(6)), 2, 6, 10, 14)
      mix(v, m(s(3)), m(s(7)), 3, 7, 11, 15)
      mix(v, m(s(8)), m(s(12)), 0, 5, 10, 15)
      mix(v, m(s(9)), m(s(13)), 1, 6, 11, 12)
      mix(v, m(s(10)), m(s(14)), 2, 7, 8, 13)
      mix(v, m(s(11)), m(s(15)), 3, 4, 9, 14)
      j += 1
    }

    // update h:
    var offset = 0
    while (offset < h.length) {
      h(offset) ^= v(offset) ^ v(offset + 8)
      offset += 1
    }
  }

  private def mix(v: Array[Long], a: Long, b: Long, i: Int, j: Int, k: Int, l: Int): Unit = {
    v(i) += a + v(j)
    v(l) = java.lang.Long.rotateLeft(v(l) ^ v(i), -32)
    v(k) += v(l)
    v(j) = java.lang.Long.rotateLeft(v(j) ^ v(k), -24)
    v(i) += b + v(j)
    v(l) = java.lang.Long.rotateLeft(v(l) ^ v(i), -16)
    v(k) += v(l)
    v(j) = java.lang.Long.rotateLeft(v(j) ^ v(k), -63)
  }
}
