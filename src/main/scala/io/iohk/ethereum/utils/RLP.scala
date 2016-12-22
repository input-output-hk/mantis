package io.iohk.ethereum.utils

import java.nio.ByteBuffer

import scala.annotation.{switch, tailrec}
import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

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

  def encode(input: RLPEncodeable): Try[Array[Byte]] = {
    input match {
      case list: RLPList =>
        val output = list.items.foldLeft(Try(Array[Byte]())) {
          (acum, item) => {
            val encoded = encode(item)
            if (acum.isSuccess && encoded.isSuccess) Success(acum.get ++ encoded.get)
            else encoded
          }
        }
        output.flatMap((o: Array[Byte]) => encodeLength(o.length, OffsetShortList)).map(p => p ++ output.get)
      case value: RLPValue =>
        val inputAsBytes = value.toBytes
        if (inputAsBytes.length == 1 && (inputAsBytes(0) & 0xff) < 0x80) Success(inputAsBytes)
        else encodeLength(inputAsBytes.length, OffsetShortItem).map(l => l ++ inputAsBytes)
    }
  }

  def decode(data: Array[Byte]): Try[RLPEncodeable] = decodeWithPos(data, 0).map(_._1)

  def encodeByte(singleByte: Byte): Array[Byte] = {
    singleByte match {
      case value if (value & 0xFF) == 0 => Array.emptyByteArray
      case value => Array(singleByte)
    }
  }

  def encodeShort(singleShort: Short): Array[Byte] = {
    if ((singleShort & 0xFF) == singleShort) encodeByte(singleShort.toByte)
    else Array[Byte]((singleShort >> 8 & 0xFF).toByte, (singleShort >> 0 & 0xFF).toByte)
  }

  def encodeInt(singleInt: Int): Array[Byte] = {
    singleInt match {
      case value if value == (value & 0xFF) => encodeByte(singleInt.toByte)
      case value if value == (value & 0xFFFF) => encodeShort(singleInt.toShort)
      case value if value == (value & 0xFFFFFF) => Array((singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)
      case value => Array((singleInt >>> 24).toByte, (singleInt >>> 16).toByte, (singleInt >>> 8).toByte, singleInt.toByte)
    }
  }

  def decodeInt(bytes: Array[Byte]): Int = {
    (bytes.length: @switch) match {
      case 0 => 0: Short
      case 1 => bytes(0) & 0xFF
      case 2 => ((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)
      case 3 => ((bytes(0) & 0xFF) << 16) + ((bytes(1) & 0xFF) << 8) + (bytes(2) & 0xFF)
      case 4 => ((bytes(0) & 0xFF) << 24) + ((bytes(1) & 0xFF) << 16) + ((bytes(2) & 0xFF) << 8) + (bytes(3) & 0xFF)
      case _ => throw new Exception("Bytes don't represent an int")
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
  private def encodeLength(length: Int, offset: Int): Try[Array[Byte]] = {
    length match {
      case l if l < SizeThreshold => Success(Array((length + offset).toByte))
      case l if l < MaxItemLength && length > 0xFF =>
        val binaryLength: Array[Byte] = intToBytesNoLeadZeroes(length)
        Success((binaryLength.length + offset + SizeThreshold - 1).toByte +: binaryLength)
      case l if l < MaxItemLength && length < 0xFF => Success(Array((1 + offset + SizeThreshold - 1).toByte, length.toByte))
      case _ => Failure(new RuntimeException("Input too long"))
    }
  }


  private def decodeWithPos(data: Array[Byte], pos: Int): Try[(RLPEncodeable, Int)] = Try {
    if (data.length < 1) (
      new RLPValue {
        override def toBytes: Array[Byte] = Array.emptyByteArray
      }, pos)
    else {
      val prefix: Int = data(pos) & 0xFF
      prefix match {
        case p if p == OffsetShortItem => (new RLPValue {
          override def toBytes: Array[Byte] = Array.emptyByteArray
        }, pos + 1)
        case p if p < OffsetShortItem => (new RLPValue {
          override def toBytes: Array[Byte] = Array(data(pos))
        }, pos + 1)
        case p if p <= OffsetLongItem =>
          val length = p - OffsetShortItem
          val res: Array[Byte] = new Array[Byte](length)
          Array.copy(data, pos + 1, res, 0, length)
          (new RLPValue {
            override def toBytes: Array[Byte] = res
          }, pos + length + 1)
        case p if p < OffsetShortList =>
          val lengthOfLength = p - OffsetLongItem
          val lengthBytes = new Array[Byte](lengthOfLength)
          Array.copy(data, pos + 1, lengthBytes, 0, lengthOfLength)
          val length = decodeInt(lengthBytes)
          val beginPos = pos + lengthOfLength + 1
          val res = new Array[Byte](length)
          Array.copy(data, beginPos, res, 0, length)
          (new RLPValue {
            override def toBytes: Array[Byte] = res
          }, beginPos + length)
        case p if p <= OffsetLongList =>
          val length = p - OffsetShortList
          val (listDecoded, endPos) = decodeListRecursive(data, pos + 1, length, Queue()).get
          (new RLPList {
            override def items = listDecoded
          }, endPos)
        case p =>
          val lengthOfLength = p - OffsetLongList
          val lengthBytes = new Array[Byte](lengthOfLength)
          Array.copy(data, pos + 1, lengthBytes, 0, lengthOfLength)
          val length = decodeInt(lengthBytes)
          val (listDecoded, endPos) = decodeListRecursive(data, pos + lengthOfLength + 1, length, Queue()).get
          (new RLPList {
            override def items = listDecoded
          }, endPos)
      }
    }
  }

  @tailrec
  private def decodeListRecursive(data: Array[Byte], pos: Int, length: Int,
                                  acum: Queue[RLPEncodeable]): Try[(Queue[RLPEncodeable], Int)] = {
    if (length == 0) Success((acum, pos))
    else {
      val maybeDecoded = decodeWithPos(data, pos)
      maybeDecoded match {
        case Success((decoded, decodedEnd)) => decodeListRecursive(data, decodedEnd, length - (decodedEnd - pos), acum :+ decoded)
        case Failure(e) => Failure(e)
      }
    }
  }
}

sealed trait RLPEncodeable

trait RLPList extends RLPEncodeable {
  def items: Seq[RLPEncodeable]

  def sameElements(other: RLPList)(): Boolean = items == other.items

  override def equals(other: Any): Boolean = other match {
    case other: RLPList => hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = items.hashCode()
}

object RLPList {
  def apply(enc: RLPEncodeable*): RLPList = new RLPList {
    override def items: Seq[RLPEncodeable] = enc.toSeq
  }

  def apply[E](seq: Seq[E])(implicit convert: E => RLPEncodeable): RLPList = new RLPList {
    override def items: Seq[RLPEncodeable] = seq.map(convert)
  }
}

trait RLPValue extends RLPEncodeable {
  def toBytes: Array[Byte]

  override def equals(other: Any): Boolean = other match {
    case other: RLPValue => hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = toBytes.toSeq.hashCode()
}
