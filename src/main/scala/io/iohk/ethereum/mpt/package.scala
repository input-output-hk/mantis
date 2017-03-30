package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.db.storage.NodeStorage

package object mpt {

  trait ByteArrayEncoder[T] {
    def toBytes(input: T): Array[Byte]
  }

  trait ByteArrayDecoder[T] {
    def fromBytes(bytes: Array[Byte]): T
  }

  trait ByteArraySerializable[T] extends ByteArrayEncoder[T] with ByteArrayDecoder[T]

  implicit val byteStringSerializer = new ByteArraySerializable[ByteString] {
    override def toBytes(input: Code): Array[Byte] = input.toArray[Byte]
    override def fromBytes(bytes: Array[Byte]): Code = ByteString(bytes)
  }
}
