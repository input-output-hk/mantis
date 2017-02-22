package io.iohk.ethereum.mpt

import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncoder}

class RLPByteArraySerializable[T](implicit val enc: RLPEncoder[T], implicit val dec: RLPDecoder[T]) extends ByteArraySerializable[T] {

  import io.iohk.ethereum.rlp.{decode, encode}

  override def toBytes(input: T): Array[Byte] = encode(input)(enc)

  override def fromBytes(bytes: Array[Byte]): T = decode[T](bytes)(dec)
}
