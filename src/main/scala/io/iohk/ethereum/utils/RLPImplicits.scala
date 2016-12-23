package io.iohk.ethereum.utils

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
    override def encode(obj: Byte): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = encodeByte(obj)
    }

    override def decode(rlp: RLPEncodeable): Byte = rlp match {
      case srcRLPValue: RLPValue =>
        val bytes = srcRLPValue.toBytes
        bytes.length match {
          case 0 => 0: Byte
          case 1 => (bytes(0) & 0xFF).toByte
          case _ => throw new Exception("src doesn't represent a byte")
        }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val shortEncDec = new RLPEncoder[Short] with RLPDecoder[Short] {
    override def encode(obj: Short): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = encodeShort(obj)
    }

    override def decode(rlp: RLPEncodeable): Short = rlp match {
      case srcRLPValue: RLPValue =>
        val bytes = srcRLPValue.toBytes
        bytes.length match {
          case 0 => 0: Short
          case 1 => (bytes(0) & 0xFF).toShort
          case 2 => (((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)).toShort
          case _ => throw new Exception("src doesn't represent a short")
        }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val intEncDec = new RLPEncoder[Int] with RLPDecoder[Int] {
    override def encode(obj: Int): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = encodeInt(obj)
    }

    override def decode(rlp: RLPEncodeable): Int = rlp match {
      case srcRLPValue: RLPValue => decodeInt(srcRLPValue.toBytes)
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val bigIntEncDec = new RLPEncoder[BigInt] with RLPDecoder[BigInt] {

    def asUnsignedByteArray(srcBigInteger: BigInt): Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray(0) == 0) asByteArray.tail
      else asByteArray
    }

    override def encode(obj: BigInt): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = if (obj.equals(BigInt(0))) encodeByte(0: Byte) else asUnsignedByteArray(obj)
    }

    override def decode(rlp: RLPEncodeable): BigInt = rlp match {
      case srcRLPValue: RLPValue => srcRLPValue.toBytes.foldLeft[BigInt](BigInt(0)) { (rec, byte) => (rec << (8: Int)) + BigInt(byte & 0xFF) }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val longEncDec = new RLPEncoder[Long] with RLPDecoder[Long] {
    override def encode(obj: Long): RLPValue = bigIntEncDec.encode(BigInt(obj))

    override def decode(rlp: RLPEncodeable): Long = bigIntEncDec.decode(rlp).toLong
  }

  implicit val stringEncDec = new RLPEncoder[String] with RLPDecoder[String] {
    override def encode(obj: String): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = obj.getBytes()
    }

    override def decode(rlp: RLPEncodeable): String = rlp match {
      case srcString: RLPValue => new String(srcString.toBytes)
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val byteArrayEncDec = new RLPEncoder[Array[Byte]] with RLPDecoder[Array[Byte]] {

    override def encode(obj: Array[Byte]): RLPValue = new RLPValue {
      override def toBytes: Array[Byte] = obj
    }

    override def decode(rlp: RLPEncodeable): Array[Byte] = rlp match {
      case srcRLPValue: RLPValue => srcRLPValue.toBytes
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit val emptyEncDec = new RLPEncoder[Nothing] {
    override def encode(obj: Nothing): RLPEncodeable = new RLPValue {
      override def toBytes: Array[Byte] = Array.emptyByteArray
    }
  }

  implicit def seqEncDec[T]()(implicit enc: RLPEncoder[T], dec: RLPDecoder[T]) = new RLPEncoder[Seq[T]] with RLPDecoder[Seq[T]] {
    override def encode(obj: Seq[T]): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = obj.map(enc.encode)
    }

    override def decode(rlp: RLPEncodeable): Seq[T] = rlp match {
      case l: RLPList => l.items.map(dec.decode)
      case _ => throw new Exception("src is not a Seq")
    }
  }

  implicit def emptySeqEncDec = new RLPEncoder[Seq[Any]] with RLPDecoder[Seq[Any]] {
    override def encode(obj: Seq[Any]): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = if (obj.isEmpty) Seq() else throw new Exception("src is not an Empty Seq")
    }

    override def decode(rlp: RLPEncodeable): Seq[Any] = rlp match {
      case l: RLPList if l.items.isEmpty => Seq()
      case _ => throw new Exception("src is not an empty Seq")
    }
  }
}
