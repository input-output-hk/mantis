package io.iohk.ethereum

package object mpt {
  trait ByteArraySerializable[T] {
    def toBytes(input: T): Array[Byte]

    def fromBytes(bytes: Array[Byte]): T
  }
}
