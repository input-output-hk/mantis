package io.iohk.ethereum.db

trait DataSource {

  type Key = Array[Byte]
  type Value = Array[Byte]

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  def get(namespace: Byte, key: Key): Option[Value]

  /**
    * This function updates the DataSource by deleting and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param toRemove which includes all the keys to be removed from the DataSource.
    * @param toUpdate which includes all the (key-value) pairs to be inserted into the DataSource.
    * @return the new DataSource after the removals and insertions were done.
    */
  def update(namespace: Byte, toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource
}
