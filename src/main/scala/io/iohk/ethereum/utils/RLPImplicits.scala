package io.iohk.ethereum.utils

import akka.util.ByteString
import io.iohk.ethereum.utils.RLP._

import scala.language.implicitConversions


trait RLPEncoder[T] {
  def encode(obj: T): RLPEncodeable
}

trait RLPDecoder[T] {
  def decode(rlp: RLPEncodeable): T
}

object RLPImplicits {

  implicit val byteEncDec = new RLPEncoder[Byte] with RLPDecoder[Byte] {
    override def encode(obj: Byte): RLPValue = RLPValue(byteToByteString(obj))

    override def decode(rlp: RLPEncodeable): Byte = rlp match {
      case RLPValue(bytes) =>
        bytes.length match {
          case 0 => 0: Byte
          case 1 => (bytes(0) & 0xFF).toByte
          case _ => throw new RuntimeException("src doesn't represent a byte")
        }
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val shortEncDec = new RLPEncoder[Short] with RLPDecoder[Short] {
    override def encode(obj: Short): RLPValue = RLPValue(shortToBigEndianMinLength(obj))

    override def decode(rlp: RLPEncodeable): Short = rlp match {
      case RLPValue(bytes) =>
        bytes.length match {
          case 0 => 0: Short
          case 1 => (bytes(0) & 0xFF).toShort
          case 2 => (((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)).toShort
          case _ => throw new RuntimeException("src doesn't represent a short")
        }
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val intEncDec = new RLPEncoder[Int] with RLPDecoder[Int] {
    override def encode(obj: Int): RLPValue = RLPValue(intToBigEndianMinLength(obj))

    override def decode(rlp: RLPEncodeable): Int = rlp match {
      case RLPValue(bytes) => bigEndianMinLengthToInt(bytes)
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val bigIntEncDec = new RLPEncoder[BigInt] with RLPDecoder[BigInt] {

    def asUnsignedByteArray(srcBigInteger: BigInt): Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray(0) == 0) asByteArray.tail
      else asByteArray
    }

    override def encode(obj: BigInt): RLPValue = RLPValue(
      if (obj.equals(BigInt(0))) byteToByteString(0: Byte) else ByteString(asUnsignedByteArray(obj))
    )

    override def decode(rlp: RLPEncodeable): BigInt = rlp match {
      case RLPValue(bytes) => bytes.foldLeft[BigInt](BigInt(0)) { (rec, byte) => (rec << (8: Int)) + BigInt(byte & 0xFF) }
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val longEncDec = new RLPEncoder[Long] with RLPDecoder[Long] {
    override def encode(obj: Long): RLPValue = bigIntEncDec.encode(BigInt(obj))

    override def decode(rlp: RLPEncodeable): Long = bigIntEncDec.decode(rlp).toLong
  }

  implicit val stringEncDec = new RLPEncoder[String] with RLPDecoder[String] {
    override def encode(obj: String): RLPValue = RLPValue(ByteString(obj))

    override def decode(rlp: RLPEncodeable): String = rlp match {
      case RLPValue(bytes) => new String(bytes.toArray)
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val byteArrayEncDec = new RLPEncoder[Array[Byte]] with RLPDecoder[Array[Byte]] {

    override def encode(obj: Array[Byte]): RLPValue = RLPValue(ByteString(obj))

    override def decode(rlp: RLPEncodeable): Array[Byte] = rlp match {
      case RLPValue(bytes) => bytes.toArray
      case _ => throw new RuntimeException("src is not an RLPValue")
    }
  }

  implicit val byteStringEncDec = new RLPEncoder[ByteString] with RLPDecoder[ByteString] {
    override def encode(obj: ByteString): RLPEncodeable = byteArrayEncDec.encode(obj.toArray[Byte])

    override def decode(rlp: RLPEncodeable): ByteString = ByteString(byteArrayEncDec.decode(rlp))
  }

  implicit val emptyEncDec = new RLPEncoder[Nothing] {
    override def encode(obj: Nothing): RLPEncodeable = RLPValue(ByteString.empty)
  }

  implicit def seqEncDec[T]()(implicit enc: RLPEncoder[T], dec: RLPDecoder[T]) = new RLPEncoder[Seq[T]] with RLPDecoder[Seq[T]] {
    override def encode(obj: Seq[T]): RLPEncodeable = RLPList(obj.map(enc.encode): _*)

    override def decode(rlp: RLPEncodeable): Seq[T] = rlp match {
      case l: RLPList => l.items.map(dec.decode)
      case _ => throw new RuntimeException("src is not a Seq")
    }
  }

  implicit def toEncodeable[T](value: T)(implicit enc: RLPEncoder[T]): RLPEncodeable = enc.encode(value)

  implicit def toEncodeableList[T](values: Seq[T])(implicit enc: RLPEncoder[T]): RLPList =
    RLPList(values.map(v => toEncodeable[T](v)): _*)

  implicit def byteFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Byte]): Byte = dec.decode(rlp)

  implicit def shortFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Short]): Short = dec.decode(rlp)

  implicit def intFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Int]): Int = dec.decode(rlp)

  implicit def bigIntFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[BigInt]): BigInt = dec.decode(rlp)

  implicit def longFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Long]): Long = dec.decode(rlp)

  implicit def stringFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[String]): String = dec.decode(rlp)

  implicit def byteArrayFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Array[Byte]]): Array[Byte] = dec.decode(rlp)

  implicit def byteStringFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[ByteString]): ByteString = dec.decode(rlp)
}