package io.iohk.ethereum

package object mpt {
  trait ByteArraySerializable[T] {
    def toBytes(input: T): Array[Byte]

    def fromBytes(bytes: Array[Byte]): T
  }

  trait DataSource {

    type Key = Array[Byte]
    type Value = Array[Byte]

    /**
      * This function obtains the associated value to a key, if there exists one.
      *
      * @param key
      * @return the value associated with the passed key
      */
    def get(key: Key): Option[Value]

    /**
      * This function updates the DataSource by deleting and inserting new (key-value) pairs.
      *
      * @param rootHash of the latest version of the trie, after updating it with the (key-value) pairs from toRemove and toUpdate.
      *                 Its purpose is to be used for version name generation if necessary (as is the case with IODB).
      * @param toRemove which includes all the keys to be removed from the DataSource.
      * @param toUpdate which includes all the (key-value) pairs to be inserted into the DataSource.
      * @return the new DataSource after the removals and insertions were done.
      */
    def update(rootHash: Array[Byte], toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource
  }
}
