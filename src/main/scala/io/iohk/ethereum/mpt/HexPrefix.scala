package io.iohk.ethereum.mpt

object HexPrefix {

  /** Pack nibbles to binary
    *
    * @param nibbles sequence
    * @param isLeaf boolean used to encode whether or not the data being encoded corresponds to a LeafNode or an ExtensionNode
    * @return hex-encoded byte array
    */
  def encode(nibbles: Array[Byte], isLeaf: Boolean): Array[Byte] = {
    val hasOddLength = nibbles.length % 2 == 1
    val firstByteFlag: Byte = (2 * (if (isLeaf) 1 else 0) + (if (hasOddLength) 1 else 0)).toByte
    val lengthFlag = if (hasOddLength) 1 else 2

    val nibblesWithFlag = new Array[Byte](nibbles.length + lengthFlag)
    Array.copy(nibbles, 0, nibblesWithFlag, lengthFlag, nibbles.length)
    nibblesWithFlag(0) = firstByteFlag
    if (!hasOddLength) nibblesWithFlag(1) = 0
    nibblesToBytes(nibblesWithFlag)
  }

  /** Unpack a binary string to its nibbles equivalent
    *
    * @param src of binary data
    * @return array of nibbles in byte-format and
    *         boolean used to encode whether or not the data being decoded corresponds to a LeafNode or an ExtensionNode
    */
  def decode(src: Array[Byte]): (Array[Byte], Boolean) = {
    val srcNibbles: Array[Byte] = bytesToNibbles(bytes = src)
    val t = (srcNibbles(0) & 2) != 0
    val hasOddLength = (srcNibbles(0) & 1) != 0
    val flagLength = if (hasOddLength) 1 else 2

    val res = new Array[Byte](srcNibbles.length - flagLength)
    Array.copy(srcNibbles, flagLength, res, 0, srcNibbles.length - flagLength)
    (res, t)
  }

  /** Transforms an array of 8bit values to the corresponding array of 4bit values (hexadecimal format)
    * Needs to be as fast possible, which requires usage of var's and mutable arrays.
    * @param bytes byte[]
    * @return array with each individual nibble
    */
  def bytesToNibbles(bytes: Array[Byte]): Array[Byte] = {
    val newArray = new Array[Byte](bytes.length * 2)
    var i = 0
    var n = 0
    while (i < bytes.length) {
      newArray(n) = ((bytes(i) >> 4) & 0xf).toByte
      newArray(n + 1) = (bytes(i) & 0xf).toByte
      n = n + 2
      i = i + 1
    }
    newArray
  }

  /** Transforms an array of 4bit values (hexadecimal format) to the corresponding array of 8bit values
    * Needs to be as fast possible, which requires usage of var's and mutable arrays.
    * @param nibbles byte[]
    * @return array with bytes combining pairs of nibbles
    */
  def nibblesToBytes(nibbles: Array[Byte]): Array[Byte] = {
    require(nibbles.length % 2 == 0)
    val newArray = new Array[Byte](nibbles.length / 2)
    var i = 0
    var n = 0

    while (i < nibbles.length) {
      val newValue = (16 * nibbles(i) + nibbles(i + 1)).toByte
      newArray(n) = newValue
      n = n + 1
      i = i + 2
    }

    newArray
  }
}
