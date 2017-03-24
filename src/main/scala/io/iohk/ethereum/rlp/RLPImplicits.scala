package io.iohk.ethereum.rlp

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.rlp.RLP._
import io.iohk.ethereum.vm.UInt256

import scala.language.implicitConversions


object RLPImplicits {

  implicit val byteEncDec = new RLPEncoder[Byte] with RLPDecoder[Byte] {
    override def encode(obj: Byte): RLPValue = RLPValue(byteToByteArray(obj))

    override def decode(rlp: RLPEncodeable): Byte = rlp match {
      case RLPValue(bytes) =>

        val len = bytes.length

        if (len == 0) 0: Byte
        else if (len == 1) (bytes(0) & 0xFF).toByte
        else throw RLPException("src doesn't represent a byte")

      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  implicit val shortEncDec = new RLPEncoder[Short] with RLPDecoder[Short] {
    override def encode(obj: Short): RLPValue = RLPValue(shortToBigEndianMinLength(obj))

    override def decode(rlp: RLPEncodeable): Short = rlp match {
      case RLPValue(bytes) =>

        val len = bytes.length

        if (len == 0) 0: Short
        else if (len == 1) (bytes(0) & 0xFF).toShort
        else if (len == 2) (((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)).toShort
        else throw RLPException("src doesn't represent a short")

      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  implicit val intEncDec = new RLPEncoder[Int] with RLPDecoder[Int] {
    override def encode(obj: Int): RLPValue = RLPValue(intToBigEndianMinLength(obj))

    override def decode(rlp: RLPEncodeable): Int = rlp match {
      case RLPValue(bytes) => bigEndianMinLengthToInt(bytes)
      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  implicit val uInt256EncDec =  new RLPEncoder[UInt256] with RLPDecoder[UInt256] {
    override def encode(obj: UInt256): RLPEncodeable =
      RLPValue(if (obj.equals(UInt256.Zero)) Array[Byte](0: Byte) else obj.bytes.dropWhile(_ == 0).toArray[Byte])

    override def decode(rlp: RLPEncodeable): UInt256 = rlp match {
      case RLPValue(bytes) => UInt256(bytes)
      case _ => throw RLPException("src is not an RLPValue")
    }

  }

  //Used for decoding and encoding positive (or 0) BigInts
  implicit val bigIntEncDec = new RLPEncoder[BigInt] with RLPDecoder[BigInt] {

    def asUnsignedByteArray(srcBigInteger: BigInt): Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray.head == 0) asByteArray.tail
      else asByteArray
    }

    override def encode(obj: BigInt): RLPValue = RLPValue(
      if (obj.equals(BigInt(0))) byteToByteArray(0: Byte) else asUnsignedByteArray(obj)
    )

    override def decode(rlp: RLPEncodeable): BigInt = rlp match {
      case RLPValue(bytes) => bytes.foldLeft[BigInt](BigInt(0)) { (rec, byte) => (rec << (8: Int)) + BigInt(byte & 0xFF) }
      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  //Used for decoding and encoding positive (or 0) longs
  implicit val longEncDec = new RLPEncoder[Long] with RLPDecoder[Long] {
    override def encode(obj: Long): RLPValue = bigIntEncDec.encode(BigInt(obj))

    override def decode(rlp: RLPEncodeable): Long = rlp match {
      case RLPValue(bytes) if bytes.length <= 8 => bigIntEncDec.decode(rlp).toLong
      case _ =>  throw RLPException("src is not an RLPValue")
    }
  }

  implicit val stringEncDec = new RLPEncoder[String] with RLPDecoder[String] {
    override def encode(obj: String): RLPValue = RLPValue(obj.getBytes)

    override def decode(rlp: RLPEncodeable): String = rlp match {
      case RLPValue(bytes) => new String(bytes)
      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  implicit val byteArrayEncDec = new RLPEncoder[Array[Byte]] with RLPDecoder[Array[Byte]] {

    override def encode(obj: Array[Byte]): RLPValue = RLPValue(obj)

    override def decode(rlp: RLPEncodeable): Array[Byte] = rlp match {
      case RLPValue(bytes) => bytes
      case _ => throw RLPException("src is not an RLPValue")
    }
  }

  implicit val byteStringEncDec = new RLPEncoder[ByteString] with RLPDecoder[ByteString] {
    override def encode(obj: ByteString): RLPEncodeable = byteArrayEncDec.encode(obj.toArray[Byte])

    override def decode(rlp: RLPEncodeable): ByteString = ByteString(byteArrayEncDec.decode(rlp))
  }

  implicit def seqEncDec[T]()(implicit enc: RLPEncoder[T], dec: RLPDecoder[T]): RLPEncoder[Seq[T]] with RLPDecoder[Seq[T]] =
    new RLPEncoder[Seq[T]] with RLPDecoder[Seq[T]] {
      override def encode(obj: Seq[T]): RLPEncodeable = RLPList(obj.map(enc.encode): _*)

      override def decode(rlp: RLPEncodeable): Seq[T] = rlp match {
        case l: RLPList => l.items.map(dec.decode)
        case _ => throw RLPException("src is not a Seq")
      }
  }



  implicit def toEncodeable[T](value: T)(implicit enc: RLPEncoder[T]): RLPEncodeable = enc.encode(value)

  def fromEncodeable[T](value: RLPEncodeable)(implicit dec: RLPDecoder[T]): T = dec.decode(value)

  implicit def toEncodeableList[T](values: Seq[T])(implicit enc: RLPEncoder[T]): RLPList =
    RLPList(values.map(v => toEncodeable[T](v)): _*)

  def fromEncodeableList[T](rlpList: RLPList)(implicit dec: RLPDecoder[T]): Seq[T] =
    rlpList.items.map(dec.decode)

  implicit def byteStringToEncodeable = toEncodeable[ByteString] _

  implicit def byteFromEncodeable = fromEncodeable[Byte] _

  implicit def shortFromEncodeable = fromEncodeable[Short] _

  implicit def intFromEncodeable = fromEncodeable[Int] _

  implicit def bigIntFromEncodeable = fromEncodeable[BigInt] _

  implicit def uInt256FromEncodeable= fromEncodeable[UInt256] _

  implicit def byteStringFromEncodeable= fromEncodeable[ByteString] _

  implicit def longFromEncodeable= fromEncodeable[Long] _

  implicit def stringFromEncodeable= fromEncodeable[String] _

  implicit def byteArrayFromEncodeable= fromEncodeable[Array[Byte]] _
}
