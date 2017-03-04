package io.iohk.ethereum.mpt

import io.iohk.ethereum.crypto.kec256

case class HashByteArraySerializable[T](tSerializer: ByteArrayEncoder[T]) extends ByteArrayEncoder[T] {
  override def toBytes(input: T): Array[Byte] = kec256(tSerializer.toBytes(input))
}
