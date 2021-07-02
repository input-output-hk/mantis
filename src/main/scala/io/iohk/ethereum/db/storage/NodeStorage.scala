package io.iohk.ethereum.db.storage

import akka.util.ByteString

import monix.reactive.Observable

import io.iohk.ethereum.db.cache.Cache
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.dataSource.DataSourceUpdateOptimized
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash

sealed trait NodesStorage extends {
  def get(key: NodeHash): Option[NodeEncoded]
  def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesStorage
  def updateCond(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)], inMemory: Boolean): NodesStorage
}

/** This class is used to store Nodes (defined in mpt/Node.scala), by using:
  *   Key: hash of the RLP encoded node
  *   Value: the RLP encoded node
  */
class NodeStorage(val dataSource: DataSource)
    extends KeyValueStorage[NodeHash, NodeEncoded, NodeStorage]
    with NodesStorage {

  val namespace: IndexedSeq[Byte] = Namespaces.NodeNamespace
  def keySerializer: NodeHash => IndexedSeq[Byte] = _.toIndexedSeq
  def keyDeserializer: IndexedSeq[Byte] => NodeHash = h => ByteString(h.toArray)
  def valueSerializer: NodeEncoded => IndexedSeq[Byte] = _.toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => NodeEncoded = _.toArray

  override def get(key: NodeHash): Option[NodeEncoded] = dataSource.getOptimized(namespace, key.toArray)

  /** This function updates the KeyValueStorage by deleting, updating and inserting new (key-value) pairs
    * in the current namespace.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStorage.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStorage.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new KeyValueStorage after the removals and insertions were done.
    */
  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodeStorage = {
    dataSource.update(
      Seq(
        DataSourceUpdateOptimized(
          namespace = Namespaces.NodeNamespace,
          toRemove = toRemove.map(_.toArray),
          toUpsert = toUpsert.map(values => values._1.toArray -> values._2)
        )
      )
    )
    apply(dataSource)
  }

  override def storageContent: Observable[Either[IterationError, (NodeHash, NodeEncoded)]] =
    dataSource.iterate(namespace).map { result =>
      result.map { case (key, value) => (ByteString.fromArrayUnsafe(key), value) }
    }

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)

  def updateCond(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)], inMemory: Boolean): NodesStorage =
    update(toRemove, toUpsert)
}

class CachedNodeStorage(val storage: NodeStorage, val cache: Cache[NodeHash, NodeEncoded])
    extends CachedKeyValueStorage[NodeHash, NodeEncoded, CachedNodeStorage]
    with NodesStorage {
  override type I = NodeStorage
  override def apply(cache: Cache[NodeHash, NodeEncoded], storage: NodeStorage): CachedNodeStorage =
    new CachedNodeStorage(storage, cache)
}

object NodeStorage {
  type NodeHash = ByteString
  type NodeEncoded = Array[Byte]
}
