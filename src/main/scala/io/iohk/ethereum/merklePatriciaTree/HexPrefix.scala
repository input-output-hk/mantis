package io.iohk.ethereum.merklePatriciaTree

object HexPrefix {
  /**
    * Pack nibbles to binary
    *
    * @param nibbles sequence
    * @return hex-encoded byte array
    *
    */
  def encode(nibbles: Array[Byte], t: Boolean): Array[Byte] = {
    val hasOddLength = (nibbles.length) % 2 == 1
    val firstByteFlag: Byte = (2*(if(t) 1 else 0) + (if(hasOddLength) 1 else 0)).toByte
    val lengthFlag = if(hasOddLength) 1 else 2

    val nibblesWithFlag = new Array[Byte](nibbles.length + lengthFlag)
    Array.copy(nibbles, 0, nibblesWithFlag, lengthFlag, nibbles.length)
    nibblesWithFlag(0) = firstByteFlag
    if(!hasOddLength) nibblesWithFlag(1) = 0
    nibblesToBytes(nibblesWithFlag)
  }

  /**
    * Unpack a binary string to its nibbles equivalent
    *
    * @param src of binary data
    * @return array of nibbles in byte-format
    *
    */
  def decode(src: Array[Byte]): (Array[Byte], Boolean) = {
    val srcNibbles: Array[Byte] = bytesToNibbles(bytes = src)
    val t = (srcNibbles(0) & 2) != 0
    val hasOddLength = (srcNibbles(0) & 1) != 0
    val flagLength = if(hasOddLength) 1 else 2

    val res = new Array[Byte](srcNibbles.length - flagLength)
    Array.copy(srcNibbles, flagLength, res, 0, srcNibbles.length-flagLength)
    (res, t)
  }

  /**
    * Transforms a binary array to hexadecimal format
    *
    * @param bytes byte[]
    * @return array with each individual nibble
    *
    */
  def bytesToNibbles(bytes: Array[Byte]): Array[Byte] = {
    bytes.foldRight[List[Byte]](List()){(elem, rec) =>
      ((elem >> 4) & 0xF).toByte :: (elem & 0xF).toByte :: rec}.toArray
  }

  /**
    * Transforms an array in hexadecimal format to a byte array
    *
    * @param nibbles byte[]
    * @return array with bytes combining pairs of nibbles
    *
    */
  def nibblesToBytes(nibbles: Array[Byte]): Array[Byte] = {
    require(nibbles.length % 2 == 0)
    (0 until nibbles.length/2).map(i => (16*nibbles(2*i) + nibbles(2*i + 1)).toByte).toArray
  }
}
