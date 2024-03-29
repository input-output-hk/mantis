package io.iohk.ethereum.rlp

import java.nio.ByteBuffer

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Recursive Length Prefix (RLP) encoding.
  * <p>
  * The purpose of RLP is to encode arbitrarily nested arrays of binary data, and
  * RLP is the main encoding method used to serialize objects in Ethereum. The
  * only purpose of RLP is to encode structure; encoding specific atomic data
  * types (eg. strings, integers, floats) is left up to higher-order protocols; in
  * Ethereum the standard is that integers are represented in big endian binary
  * form. If one wishes to use RLP to encode a dictionary, the two suggested
  * canonical forms are to either use &#91;&#91;k1,v1],[k2,v2]...] with keys in
  * lexicographic order or to use the higher-level Patricia Tree encoding as
  * Ethereum does.
  * <p>
  * The RLP encoding function takes in an item. An item is defined as follows:
  * <p>
  * - A string (ie. byte array) is an item - A list of items is an item
  * <p>
  * For example, an empty string is an item, as is the string containing the word
  * "cat", a list containing any number of strings, as well as more complex data
  * structures like ["cat",["puppy","cow"],"horse",[[]],"pig",[""],"sheep"]. Note
  * that in the context of the rest of this article, "string" will be used as a
  * synonym for "a certain number of bytes of binary data"; no special encodings
  * are used and no knowledge about the content of the strings is implied.
  * <p>
  * See: https://github.com/ethereum/wiki/wiki/%5BEnglish%5D-RLP
  */

private[rlp] object RLP {

  /** Reason for threshold according to Vitalik Buterin:
    * - 56 bytes maximizes the benefit of both options
    * - if we went with 60 then we would have only had 4 slots for long strings
    * so RLP would not have been able to store objects above 4gb
    * - if we went with 48 then RLP would be fine for 2^128 space, but that's way too much
    * - so 56 and 2^64 space seems like the right place to put the cutoff
    * - also, that's where Bitcoin's varint does the cutof
    */
  private val SizeThreshold: Int = 56

  /** Allow for content up to size of 2&#94;64 bytes *
    */
  private val MaxItemLength: Double = Math.pow(256, 8)

  /** RLP encoding rules are defined as follows: */

  /*
   * For a single byte whose value is in the [0x00, 0x7f] range, that byte is
   * its own RLP encoding.
   */

  /** [0x80]
    * If a string is 0-55 bytes long, the RLP encoding consists of a single
    * byte with value 0x80 plus the length of the string followed by the
    * string. The range of the first byte is thus [0x80, 0xb7].
    */
  private val OffsetShortItem: Int = 0x80

  /** [0xb7]
    * If a string is more than 55 bytes long, the RLP encoding consists of a
    * single byte with value 0xb7 plus the length of the length of the string
    * in binary form, followed by the length of the string, followed by the
    * string. For example, a length-1024 string would be encoded as
    * \xb9\x04\x00 followed by the string. The range of the first byte is thus
    * [0xb8, 0xbf].
    */
  private val OffsetLongItem: Int = 0xb7

  /** [0xc0]
    * If the total payload of a list (i.e. the combined length of all its
    * items) is 0-55 bytes long, the RLP encoding consists of a single byte
    * with value 0xc0 plus the length of the list followed by the concatenation
    * of the RLP encodings of the items. The range of the first byte is thus
    * [0xc0, 0xf7].
    */
  private val OffsetShortList: Int = 0xc0

  /** [0xf7]
    * If the total payload of a list is more than 55 bytes long, the RLP
    * encoding consists of a single byte with value 0xf7 plus the length of the
    * length of the list in binary form, followed by the length of the list,
    * followed by the concatenation of the RLP encodings of the items. The
    * range of the first byte is thus [0xf8, 0xff].
    */
  private val OffsetLongList = 0xf7

  /** This functions decodes an RLP encoded Array[Byte] without converting it to any specific type. This method should
    * be faster (as no conversions are done)
    *
    * @param data RLP Encoded instance to be decoded
    * @return A RLPEncodeable
    * @throws RLPException if there is any error
    */
  private[rlp] def rawDecode(data: Array[Byte]): RLPEncodeable = decodeWithPos(data, 0)._1

  /** This function encodes an RLPEncodeable instance
    *
    * @param input RLP Instance to be encoded
    * @return A byte array with item encoded
    */
  private[rlp] def encode(input: RLPEncodeable): Array[Byte] =
    input match {
      case list: RLPList =>
        val output = list.items.foldLeft(Array[Byte]())((acum, item) => acum ++ encode(item))
        encodeLength(output.length, OffsetShortList) ++ output
      case value: RLPValue =>
        val inputAsBytes = value.bytes
        if (inputAsBytes.length == 1 && (inputAsBytes(0) & 0xff) < 0x80) inputAsBytes
        else encodeLength(inputAsBytes.length, OffsetShortItem) ++ inputAsBytes
      case PrefixedRLPEncodable(prefix, prefixedRLPEncodeable) =>
        prefix +: encode(prefixedRLPEncodeable)
    }

  /** This function transform a byte into byte array
    *
    * @param singleByte to encode
    * @return encoded bytes
    */
  private[rlp] def byteToByteArray(singleByte: Byte): Array[Byte] =
    if ((singleByte & 0xff) == 0) Array.emptyByteArray
    else Array[Byte](singleByte)

  /** This function converts a short value to a big endian byte array of minimal length
    *
    * @param singleShort value to encode
    * @return encoded bytes
    */
  private[rlp] def shortToBigEndianMinLength(singleShort: Short): Array[Byte] =
    if ((singleShort & 0xff) == singleShort) byteToByteArray(singleShort.toByte)
    else Array[Byte]((singleShort >> 8 & 0xff).toByte, (singleShort >> 0 & 0xff).toByte)

  /** This function converts an int value to a big endian byte array of minimal length
    *
    * @param singleInt value to encode
    * @return encoded bytes
    */
  private[rlp] def intToBigEndianMinLength(singleInt: Int): Array[Byte] =
    if (singleInt == (singleInt & 0xff)) byteToByteArray(singleInt.toByte)
    else if (singleInt == (singleInt & 0xffff)) shortToBigEndianMinLength(singleInt.toShort)
    else if (singleInt == (singleInt & 0xffffff))
      Array[Byte]((singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)
    else Array[Byte]((singleInt >>> 24).toByte, (singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)

  /** This function converts from a big endian byte array of minimal length to an int value
    *
    * @param bytes encoded bytes
    * @return Int value
    * @throws RLPException If the value cannot be converted to a valid int
    */
  private[rlp] def bigEndianMinLengthToInt(bytes: Array[Byte]): Int =
    (bytes.length: @switch) match {
      case 0 => 0: Short
      case 1 => bytes(0) & 0xff
      case 2 => ((bytes(0) & 0xff) << 8) + (bytes(1) & 0xff)
      case 3 => ((bytes(0) & 0xff) << 16) + ((bytes(1) & 0xff) << 8) + (bytes(2) & 0xff)
      case Integer.BYTES =>
        ((bytes(0) & 0xff) << 24) + ((bytes(1) & 0xff) << 16) + ((bytes(2) & 0xff) << 8) + (bytes(3) & 0xff)
      case _ => throw RLPException("Bytes don't represent an int")
    }

  /** Converts a int value into a byte array.
    *
    * @param value - int value to convert
    * @return value with leading byte that are zeroes striped
    */
  private def intToBytesNoLeadZeroes(value: Int): Array[Byte] =
    ByteBuffer.allocate(Integer.BYTES).putInt(value).array().dropWhile(_ == (0: Byte))

  /** Integer limitation goes up to 2&#94;31-1 so length can never be bigger than MAX_ITEM_LENGTH
    */
  private def encodeLength(length: Int, offset: Int): Array[Byte] =
    if (length < SizeThreshold) Array((length + offset).toByte)
    else if (length < MaxItemLength && length > 0xff) {
      val binaryLength: Array[Byte] = intToBytesNoLeadZeroes(length)
      (binaryLength.length + offset + SizeThreshold - 1).toByte +: binaryLength
    } else if (length < MaxItemLength && length <= 0xff) Array((1 + offset + SizeThreshold - 1).toByte, length.toByte)
    else throw RLPException("Input too long")

  /** This function calculates, based on RLP definition, the bounds of a single value.
    *
    * @param data An Array[Byte] containing the RLP item to be searched
    * @param pos  Initial position to start searching
    * @return Item Bounds description
    * @see [[io.iohk.ethereum.rlp.ItemBounds]]
    */
  private[rlp] def getItemBounds(data: Array[Byte], pos: Int): ItemBounds =
    if (data.isEmpty) throw RLPException("Empty Data")
    else {
      val prefix: Int = data(pos) & 0xff
      if (prefix == OffsetShortItem) {
        ItemBounds(start = pos, end = pos, isList = false, isEmpty = true)
      } else if (prefix < OffsetShortItem)
        ItemBounds(start = pos, end = pos, isList = false)
      else if (prefix <= OffsetLongItem) {
        val length = prefix - OffsetShortItem
        ItemBounds(start = pos + 1, end = pos + length, isList = false)
      } else if (prefix < OffsetShortList) {
        val lengthOfLength = prefix - OffsetLongItem
        val lengthBytes = data.slice(pos + 1, pos + 1 + lengthOfLength)
        val length = bigEndianMinLengthToInt(lengthBytes)
        val beginPos = pos + 1 + lengthOfLength
        ItemBounds(start = beginPos, end = beginPos + length - 1, isList = false)
      } else if (prefix <= OffsetLongList) {
        val length = prefix - OffsetShortList
        ItemBounds(start = pos + 1, end = pos + length, isList = true)
      } else {
        val lengthOfLength = prefix - OffsetLongList
        val lengthBytes = data.slice(pos + 1, pos + 1 + lengthOfLength)
        val length = bigEndianMinLengthToInt(lengthBytes)
        val beginPos = pos + 1 + lengthOfLength
        ItemBounds(start = beginPos, end = beginPos + length - 1, isList = true)
      }
    }

  private def decodeWithPos(data: Array[Byte], pos: Int): (RLPEncodeable, Int) =
    if (data.isEmpty) throw RLPException("data is too short")
    else {
      getItemBounds(data, pos) match {
        case ItemBounds(start, end, false, isEmpty) =>
          RLPValue(if (isEmpty) Array.emptyByteArray else data.slice(start, end + 1)) -> (end + 1)
        case ItemBounds(start, end, true, _) =>
          RLPList(decodeListRecursive(data, start, end - start + 1, Queue()): _*) -> (end + 1)
      }
    }

  @tailrec
  private def decodeListRecursive(
      data: Array[Byte],
      pos: Int,
      length: Int,
      acum: Queue[RLPEncodeable]
  ): (Queue[RLPEncodeable]) =
    if (length == 0) acum
    else {
      val (decoded, decodedEnd) = decodeWithPos(data, pos)
      decodeListRecursive(data, decodedEnd, length - (decodedEnd - pos), acum :+ decoded)
    }
}

private case class ItemBounds(start: Int, end: Int, isList: Boolean, isEmpty: Boolean = false)
