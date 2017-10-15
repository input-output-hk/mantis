package io.iohk.ethereum.db.dataSource

import io.iohk.ethereum.common.{BatchOperation, Removal, Upsert}
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}

class EphemDataSource(var storage: Map[IndexedSeq[Byte], IndexedSeq[Byte]]) extends DataSource {

  /**
    * key.drop to remove namespace prefix from the key
    * @return key values paris from this storage
    */
  def getAll(namespace: Namespace): Seq[(IndexedSeq[Byte], IndexedSeq[Byte])] =
    storage.toSeq.map{case (key, value) => (key.drop(namespace.length), value)}

  override def get(namespace: Namespace, key: Key): Option[Value] = storage.get(namespace ++: key)

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param batchOperations sequence of operations to be applied
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(namespace: Namespace, batchOperations: Seq[BatchOperation[Key, Value]]): DataSource = {
    val afterUpdate = batchOperations.foldLeft(storage) { (storage, bOp) =>
      bOp match {
        case Removal(key) => storage - (namespace ++ key)
        case Upsert(key, value) => storage + ((namespace ++ key) -> value)
      }
    }

    storage = afterUpdate
    this
  }

  override def clear: DataSource = {
    storage = Map()
    this
  }

  override def close(): Unit = ()

  override def destroy(): Unit = ()

}

object EphemDataSource {
  def apply(): EphemDataSource = new EphemDataSource(Map())
}
