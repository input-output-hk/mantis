package io.iohk.ethereum.db.dataSource

trait DataSource {
  import DataSource._

  /**
    * This function obtains the associated value to a key. It requires the (key-value) pair to be in the DataSource
    *
    * @param namespace which will be searched for the key.
    * @param key the key retrieve the value.
    * @return the value associated with the passed key.
    */
  def apply(namespace: Namespace, key: Key): Value = get(namespace, key).get

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key the key retrieve the value.
    * @return the value associated with the passed key.
    */
  def get(namespace: Namespace, key: Key): Option[Value]

  /**
    * This function obtains the associated value to a key, if there exists one. It assumes that
    * caller already properly serialized key. Useful when caller knows some pattern in data to
    * avoid generic serialization.
    *
    * @param key the key retrieve the value.
    * @return the value associated with the passed key.
    */
  def getOptimized(key: Array[Byte]): Option[Array[Byte]]

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param toRemove which includes all the keys to be removed from the DataSource.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the DataSource.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    * It assumes that caller already properly serialized key and value.
    * Useful when caller knows some pattern in data to avoid generic serialization.
    *
    * @param toRemove which includes all the keys to be removed from the DataSource.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the DataSource.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  def updateOptimized(toRemove: Seq[Array[Byte]], toUpsert: Seq[(Array[Byte], Array[Byte])]): DataSource

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
