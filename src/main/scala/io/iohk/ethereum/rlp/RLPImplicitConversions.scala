package io.iohk.ethereum.rlp

import akka.util.ByteString
import io.iohk.ethereum.rlp.RLPImplicits._

import scala.language.implicitConversions

object RLPImplicitConversions {

  implicit def toEncodeable[T](value: T)(implicit enc: RLPEncoder[T]): RLPEncodeable = enc.encode(value)

  def fromEncodeable[T](value: RLPEncodeable)(implicit dec: RLPDecoder[T]): T = dec.decode(value)

  implicit def toRlpList[T](values: Seq[T])(implicit enc: RLPEncoder[T]): RLPList =
    RLPList(values.map(v => toEncodeable[T](v)): _*)

  def fromRlpList[T](rlpList: RLPList)(implicit dec: RLPDecoder[T]): Seq[T] =
    rlpList.items.map(dec.decode)

  implicit def byteStringToEncodeable: (ByteString) => RLPEncodeable = toEncodeable[ByteString]

  implicit def byteFromEncodeable: (RLPEncodeable) => Byte = fromEncodeable[Byte]

  implicit def shortFromEncodeable: (RLPEncodeable) => Short = fromEncodeable[Short]

  implicit def intFromEncodeable: (RLPEncodeable) => Int = fromEncodeable[Int]

  implicit def bigIntFromEncodeable: (RLPEncodeable) => BigInt = fromEncodeable[BigInt]

  implicit def byteStringFromEncodeable: (RLPEncodeable) => ByteString = fromEncodeable[ByteString]

  implicit def longFromEncodeable: (RLPEncodeable) => Long = fromEncodeable[Long]

  implicit def stringFromEncodeable: (RLPEncodeable) => String = fromEncodeable[String]

  implicit def byteArrayFromEncodeable: (RLPEncodeable) => Array[Byte] = fromEncodeable[Array[Byte]]
}
