package io.iohk.ethereum.utils

import java.nio.ByteBuffer

import akka.util.ByteString

import scala.annotation.{switch, tailrec}
import scala.collection.immutable.Queue

/**
  * Recursive Length Prefix (RLP) encoding.
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
  *
  */

object RLP {
  /**
    * Reason for threshold according to Vitalik Buterin:
    * - 56 bytes maximizes the benefit of both options
    * - if we went with 60 then we would have only had 4 slots for long strings
    * so RLP would not have been able to store objects above 4gb
    * - if we went with 48 then RLP would be fine for 2^128 space, but that's way too much
    * - so 56 and 2^64 space seems like the right place to put the cutoff
    * - also, that's where Bitcoin's varint does the cutof
    */
  val SizeThreshold: Int = 56

  /**
    * Allow for content up to size of 2&#94;64 bytes *
    **/
  val MaxItemLength: Double = Math.pow(256, 8)

  /** RLP encoding rules are defined as follows: */

  /*
   * For a single byte whose value is in the [0x00, 0x7f] range, that byte is
   * its own RLP encoding.
   */

  /**
    * [0x80]
    * If a string is 0-55 bytes long, the RLP encoding consists of a single
    * byte with value 0x80 plus the length of the string followed by the
    * string. The range of the first byte is thus [0x80, 0xb7].
    */
  val OffsetShortItem: Int = 0x80

  /**
    * [0xb7]
    * If a string is more than 55 bytes long, the RLP encoding consists of a
    * single byte with value 0xb7 plus the length of the length of the string
    * in binary form, followed by the length of the string, followed by the
    * string. For example, a length-1024 string would be encoded as
    * \xb9\x04\x00 followed by the string. The range of the first byte is thus
    * [0xb8, 0xbf].
    */
  val OffsetLongItem: Int = 0xb7

  /**
    * [0xc0]
    * If the total payload of a list (i.e. the combined length of all its
    * items) is 0-55 bytes long, the RLP encoding consists of a single byte
    * with value 0xc0 plus the length of the list followed by the concatenation
    * of the RLP encodings of the items. The range of the first byte is thus
    * [0xc0, 0xf7].
    */
  val OffsetShortList: Int = 0xc0

  /**
    * [0xf7]
    * If the total payload of a list is more than 55 bytes long, the RLP
    * encoding consists of a single byte with value 0xf7 plus the length of the
    * length of the list in binary form, followed by the length of the list,
    * followed by the concatenation of the RLP encodings of the items. The
    * range of the first byte is thus [0xf8, 0xff].
    */
  val OffsetLongList = 0xf7

  def encode[T](input: T)(implicit enc: RLPEncoder[T]): Array[Byte] = encode(enc.encode(input))

  def decode[T](data: Array[Byte])(implicit dec: RLPDecoder[T]): T = dec.decode(rawDecode(data))

  /**
    * This function calculates the next element item based on a previos element starting position. It's meant to be
    * used while decoding a stream of RLPEncoded Items.
    *
    * @param data Data with encoded items
    * @param pos  Where to start. This value should be a valid start element position in order to be able to calculate
    *             next one
    * @return Next item position
    * @throws RuntimeException if there is any error
    */
  def nextElementIndex(data: Array[Byte], pos: Int): Int = getItemBounds(ByteString(data), pos).end + 1

  /**
    * This functions decodes an RLP encoded Array[Byte] without converting it to any specific type. This method should
    * be faster (as no conversions are done)
    *
    * @param data RLP Encoded instance to be decoded
    * @return A RLPEncodeable
    * @throws RuntimeException if there is any error
    */
  def rawDecode(data: Array[Byte]): RLPEncodeable = decodeWithPos(ByteString(data), 0)._1

  /**
    * This function encodes an RLPEncodeable instance
    *
    * @param input RLP Instance to be encoded
    * @return A byte array with item encoded
    */
  def encode(input: RLPEncodeable): Array[Byte] = {
    input match {
      case list: RLPList =>
        val output = list.items.foldLeft(Array[Byte]()) { (acum, item) => acum ++ encode(item) }
        encodeLength(output.length, OffsetShortList) ++ output
      case value: RLPValue =>
        val inputAsBytes = value.bytes.toArray
        if (inputAsBytes.length == 1 && (inputAsBytes(0) & 0xff) < 0x80) inputAsBytes
        else encodeLength(inputAsBytes.length, OffsetShortItem) ++ inputAsBytes
    }
  }

  /**
    * This function transform a byte into byte array
    *
    * @param singleByte to encode
    * @return encoded byte string
    */
  private[utils] def byteToByteString(singleByte: Byte): ByteString = {
    singleByte match {
      case value if (value & 0xFF) == 0 => ByteString.empty
      case value => ByteString(singleByte)
    }
  }

  /**
    * This function converts a short value to a big endian byte array of minimal length
    *
    * @param singleShort value to encode
    * @return encoded byte string
    */
  private[utils] def shortToBigEndianMinLength(singleShort: Short): ByteString = {
    if ((singleShort & 0xFF) == singleShort) byteToByteString(singleShort.toByte)
    else ByteString((singleShort >> 8 & 0xFF).toByte, (singleShort >> 0 & 0xFF).toByte)
  }

  /**
    * This function converts an int value to a big endian byte array of minimal length
    *
    * @param singleInt value to encode
    * @return encoded byte string
    */
  private[utils] def intToBigEndianMinLength(singleInt: Int): ByteString = {
    singleInt match {
      case value if value == (value & 0xFF) => byteToByteString(singleInt.toByte)
      case value if value == (value & 0xFFFF) => shortToBigEndianMinLength(singleInt.toShort)
      case value if value == (value & 0xFFFFFF) => ByteString((singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)
      case value => ByteString((singleInt >>> 24).toByte, (singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)
    }
  }

  /**
    * This function converts from a big endian byte array of minimal length to an int value
    *
    * @param bytes encoded bytes
    * @return Int value
    * @throws RuntimeException If the value cannot be converted to a valid int
    */
  private[utils] def bigEndianMinLengthToInt(bytes: ByteString): Int = {
    (bytes.length: @switch) match {
      case 0 => 0: Short
      case 1 => bytes(0) & 0xFF
      case 2 => ((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)
      case 3 => ((bytes(0) & 0xFF) << 16) + ((bytes(1) & 0xFF) << 8) + (bytes(2) & 0xFF)
      case 4 => ((bytes(0) & 0xFF) << 24) + ((bytes(1) & 0xFF) << 16) + ((bytes(2) & 0xFF) << 8) + (bytes(3) & 0xFF)
      case _ => throw new RuntimeException("Bytes don't represent an int")
    }
  }

  /**
    * Converts a int value into a byte array.
    *
    * @param value - int value to convert
    * @return value with leading byte that are zeroes striped
    */
  private def intToBytesNoLeadZeroes(value: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(value).array().dropWhile(_ == (0: Byte))

  /**
    * Integer limitation goes up to 2&#94;31-1 so length can never be bigger than MAX_ITEM_LENGTH
    **/
  private def encodeLength(length: Int, offset: Int): Array[Byte] = {
    length match {
      case l if l < SizeThreshold => Array((length + offset).toByte)
      case l if l < MaxItemLength && length > 0xFF =>
        val binaryLength: Array[Byte] = intToBytesNoLeadZeroes(length)
        (binaryLength.length + offset + SizeThreshold - 1).toByte +: binaryLength
      case l if l < MaxItemLength && length <= 0xFF => Array((1 + offset + SizeThreshold - 1).toByte, length.toByte)
      case _ => throw new RuntimeException("Input too long")
    }
  }

  /**
    * This function calculates, based on RLP definition, the bounds of a single value.
    *
    * @param data A byteString containing the RLP item to be searched
    * @param pos  Initial position to start searching
    * @return Item Bounds description
    * @see [[io.iohk.ethereum.utils.ItemBounds]]
    */
  private def getItemBounds(data: ByteString, pos: Int): ItemBounds =
  if (data.length < 1) throw new RuntimeException("Empty Data")
  else {
    val prefix: Int = data(pos) & 0xFF
    prefix match {
      case p if p == OffsetShortItem => ItemBounds(start = pos, end = pos, isList = false, isEmpty = true)
      case p if p < OffsetShortItem => ItemBounds(start = pos, end = pos, isList = false)
      case p if p <= OffsetLongItem =>
        val length = p - OffsetShortItem
        ItemBounds(start = pos + 1, end = pos + length, isList = false)
      case p if p < OffsetShortList =>
        val lengthOfLength = p - OffsetLongItem
        val lengthBytes = data.slice(pos + 1, pos + 1 + lengthOfLength)
        val length = bigEndianMinLengthToInt(lengthBytes)
        val beginPos = pos + 1 + lengthOfLength
        ItemBounds(start = beginPos, end = beginPos + length - 1, isList = false)
      case p if p <= OffsetLongList =>
        val length = p - OffsetShortList
        ItemBounds(start = pos + 1, end = pos + length, isList = true)
      case p =>
        val lengthOfLength = p - OffsetLongList
        val lengthBytes = data.slice(pos + 1, pos + 1 + lengthOfLength)
        val length = bigEndianMinLengthToInt(lengthBytes)
        val beginPos = pos + 1 + lengthOfLength
        ItemBounds(start = beginPos, end = beginPos + length - 1, isList = true)
    }
  }

  private def decodeWithPos(data: ByteString, pos: Int): (RLPEncodeable, Int) =
    if (data.length < 1) RLPValue(ByteString.empty) -> pos
    else {
      getItemBounds(data, pos) match {
        case ItemBounds(start, end, false, isEmpty) =>
          RLPValue(if (isEmpty) ByteString.empty else data.slice(start, end + 1)) -> (end + 1)
        case ItemBounds(start, end, true, _) =>
          RLPList(decodeListRecursive(data, start, end - start + 1, Queue()): _*) -> (end + 1)
      }
    }


  @tailrec
  private def decodeListRecursive(data: ByteString, pos: Int, length: Int,
                                  acum: Queue[RLPEncodeable]): (Queue[RLPEncodeable]) = {
    if (length == 0) acum
    else {
      val (decoded, decodedEnd) = decodeWithPos(data, pos)
      decodeListRecursive(data, decodedEnd, length - (decodedEnd - pos), acum :+ decoded)
    }
  }
}

case class ItemBounds(start: Int, end: Int, isList: Boolean, isEmpty: Boolean = false)

sealed trait RLPEncodeable

case class RLPList(items: RLPEncodeable*) extends RLPEncodeable

case class RLPValue(bytes: ByteString) extends RLPEncodeable