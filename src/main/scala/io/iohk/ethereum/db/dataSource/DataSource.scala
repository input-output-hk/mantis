package io.iohk.ethereum.db.dataSource

import io.iohk.ethereum.common.BatchOperation
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}

trait DataSource {

  /**
    * This function obtains the associated value to a key. It requires the (key-value) pair to be in the DataSource
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  def apply(namespace: Namespace, key: Key): Value = get(namespace, key).get

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  def get(namespace: Namespace, key: Key): Option[Value]

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param batchOperations sequence of operations to be applied
    * @return the new DataSource after the removals and insertions were done.
    */
  def update(namespace: Namespace, batchOperations: Seq[BatchOperation[DataSource.Key, DataSource.Value]]): DataSource

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    *
    * @return the new DataSource after all the data was removed.
    */
  def clear: DataSource

  /**
    * This function closes the DataSource, without deleting the files used by it.
    */
  def close(): Unit

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  def destroy(): Unit
}

object DataSource {

  type Key = IndexedSeq[Byte]
  type Value = IndexedSeq[Byte]
  type Namespace = IndexedSeq[Byte]
}

