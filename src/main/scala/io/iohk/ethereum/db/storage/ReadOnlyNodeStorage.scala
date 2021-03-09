package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage

import scala.collection.mutable

/** This storage allows to read from another NodesKeyValueStorage but doesn't remove or upsert into database.
  * To do so, it uses an internal in memory cache to apply all the changes.
  */
class ReadOnlyNodeStorage private (wrapped: NodesKeyValueStorage) extends NodesKeyValueStorage {
  val buffer: mutable.Map[NodeHash,Option[NodeEncoded]] = mutable.Map.empty[NodeHash, Option[NodeEncoded]]

  private def changes: (Seq[NodeHash], Seq[(NodeHash, NodeEncoded)]) =
    buffer.foldLeft(Seq.empty[NodeHash] -> Seq.empty[(NodeHash, NodeEncoded)]) { (acc, cachedItem) =>
      cachedItem match {
        case (key, Some(value)) => (acc._1, acc._2 :+ key -> value)
        case (key, None)        => (acc._1 :+ key, acc._2)
      }
    }

  /** This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: NodeHash): Option[NodeEncoded] = buffer.getOrElse(key, wrapped.get(key))

  /** This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    toRemove.foreach(elementToRemove => buffer -= elementToRemove)
    toUpsert.foreach { case (toUpsertKey, toUpsertValue) => buffer += (toUpsertKey -> Some(toUpsertValue)) }
    this
  }

  override def persist(): Unit = {
    val (toRemove, toUpsert) = changes
    wrapped.update(toRemove, toUpsert)
    buffer.clear()
  }
}

object ReadOnlyNodeStorage {
  def apply(nodesKeyValueStorage: NodesKeyValueStorage): ReadOnlyNodeStorage = new ReadOnlyNodeStorage(
    nodesKeyValueStorage
  )
}
