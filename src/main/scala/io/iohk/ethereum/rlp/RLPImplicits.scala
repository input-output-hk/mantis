package io.iohk.ethereum.rlp

import akka.util.ByteString
import io.iohk.ethereum.rlp.RLP._
import io.iohk.ethereum.utils.BigIntExtensionMethods.BigIntAsUnsigned

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

  //Used for decoding and encoding positive (or 0) BigInts
  implicit val bigIntEncDec = new RLPEncoder[BigInt] with RLPDecoder[BigInt] {

    override def encode(obj: BigInt): RLPValue = RLPValue(
      if (obj.equals(BigInt(0))) byteToByteArray(0: Byte) else obj.toUnsignedByteArray
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

}
