package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.ledger.InMemorySimpleMapProxy
import io.iohk.ethereum.mpt.NodesKeyValueStorage

/**
  * This storage allows to read from another NodesKeyValueStorage but doesn't remove or upsert into database.
  * To do so, it uses an internal in memory cache to apply all the changes.
  */
class ReadOnlyNodeStorage private(wrapped: InMemorySimpleMapProxy[NodeHash, NodeEncoded, NodesKeyValueStorage]) extends NodesKeyValueStorage {

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: NodeHash): Option[NodeEncoded] = wrapped.get(key)

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    val updatedCache = wrapped.update(toRemove, toUpsert)
    new ReadOnlyNodeStorage(updatedCache)
  }

}

object ReadOnlyNodeStorage {
  def apply(nodesKeyValueStorage: NodesKeyValueStorage): ReadOnlyNodeStorage = new ReadOnlyNodeStorage(InMemorySimpleMapProxy.wrap(nodesKeyValueStorage))
}
