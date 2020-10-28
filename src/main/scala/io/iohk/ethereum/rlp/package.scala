package io.iohk.ethereum

import akka.util.ByteString
import org.bouncycastle.util.encoders.Hex
import scala.reflect.ClassTag

package object rlp {

  case class RLPException(message: String) extends RuntimeException(message)

  sealed trait RLPEncodeable

  case class RLPList(items: RLPEncodeable*) extends RLPEncodeable {
    def ::(item: RLPEncodeable): RLPList =
      RLPList((item :: items.toList): _*)
  }

  case class RLPValue(bytes: Array[Byte]) extends RLPEncodeable {
    override def toString: String = s"RLPValue(${Hex.toHexString(bytes)})"
  }

  trait RLPEncoder[T] {
    def encode(obj: T): RLPEncodeable
  }
  object RLPEncoder {
    def apply[T](implicit ev: RLPEncoder[T]): RLPEncoder[T] = ev

    def instance[T](f: T => RLPEncodeable): RLPEncoder[T] =
      new RLPEncoder[T] {
        override def encode(obj: T): RLPEncodeable = f(obj)
      }

    def encode[T: RLPEncoder](obj: T): RLPEncodeable =
      RLPEncoder[T].encode(obj)
  }

  trait RLPDecoder[T] {
    def decode(rlp: RLPEncodeable): T
  }
  object RLPDecoder {
    def apply[T](implicit ev: RLPDecoder[T]): RLPDecoder[T] = ev

    def instance[T](f: RLPEncodeable => T): RLPDecoder[T] =
      new RLPDecoder[T] {
        override def decode(rlp: RLPEncodeable): T = f(rlp)
      }

    def decode[T: RLPDecoder](rlp: RLPEncodeable): T =
      RLPDecoder[T].decode(rlp)
  }

  def encode[T](input: T)(implicit enc: RLPEncoder[T]): Array[Byte] = RLP.encode(enc.encode(input))

  def encode(input: RLPEncodeable): Array[Byte] = RLP.encode(input)

  def decode[T](data: Array[Byte])(implicit dec: RLPDecoder[T]): T = dec.decode(RLP.rawDecode(data))

  def decode[T](data: RLPEncodeable)(implicit dec: RLPDecoder[T]): T = dec.decode(data)

  def rawDecode(input: Array[Byte]): RLPEncodeable = RLP.rawDecode(input)

  /**
    * This function calculates the next element item based on a previous element starting position. It's meant to be
    * used while decoding a stream of RLPEncoded Items.
    *
    * @param data Data with encoded items
    * @param pos  Where to start. This value should be a valid start element position in order to be able to calculate
    *             next one
    * @return Next item position
    * @throws RLPException if there is any error
    */
  def nextElementIndex(data: Array[Byte], pos: Int): Int = RLP.getItemBounds(data, pos).end + 1

  trait RLPSerializable {
    def toRLPEncodable: RLPEncodeable
    def toBytes(implicit di: DummyImplicit): ByteString = ByteString(toBytes: Array[Byte])
    def toBytes: Array[Byte] = encode(this.toRLPEncodable)
  }

  type RLPCodec[T] = RLPEncoder[T] with RLPDecoder[T]

  object RLPCodec {
    def instance[T](enc: T => RLPEncodeable, dec: PartialFunction[RLPEncodeable, T])(implicit
        ct: ClassTag[T]
    ): RLPCodec[T] =
      new RLPEncoder[T] with RLPDecoder[T] {
        override def encode(obj: T): RLPEncodeable =
          enc(obj)

        override def decode(rlp: RLPEncodeable): T =
          if (dec.isDefinedAt(rlp)) dec(rlp)
          else throw new RuntimeException(s"Cannot decode ${ct.getClass.getSimpleName} from RLP.")
      }

    def apply[T](enc: RLPEncoder[T], dec: RLPDecoder[T]): RLPCodec[T] =
      new RLPEncoder[T] with RLPDecoder[T] {
        override def encode(obj: T): RLPEncodeable = enc.encode(obj)
        override def decode(rlp: RLPEncodeable): T = dec.decode(rlp)
      }
  }
}
