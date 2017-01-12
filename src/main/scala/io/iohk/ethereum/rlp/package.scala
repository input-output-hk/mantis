package io.iohk.ethereum

package object rlp {

  case class RLPException(message: String) extends RuntimeException(message)

  sealed trait RLPEncodeable

  case class RLPList(items: RLPEncodeable*) extends RLPEncodeable

  case class RLPValue(bytes: Array[Byte]) extends RLPEncodeable

  trait RLPEncoder[T] {
    def encode(obj: T): RLPEncodeable
  }

  trait RLPDecoder[T] {
    def decode(rlp: RLPEncodeable): T
  }

  def encode[T](input: T)(implicit enc: RLPEncoder[T]): Array[Byte] = RLP.encode(enc.encode(input))

  def encode(input: RLPEncodeable): Array[Byte] = RLP.encode(input)

  def decode[T](data: Array[Byte])(implicit dec: RLPDecoder[T]): T = dec.decode(RLP.rawDecode(data))

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
}