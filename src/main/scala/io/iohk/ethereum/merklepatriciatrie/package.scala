package io.iohk.ethereum

package object merklepatriciatrie {
  trait ByteArraySerializable[T] {
    def toBytes(input: T): Array[Byte]

    def fromBytes(bytes: Array[Byte]): T
  }

  trait DataSource {

    type Key = Array[Byte]
    type Value = Array[Byte]

    def get(key: Key): Option[Value]

    def update(rootHash: Array[Byte], toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource
  }
}
