package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.cache.Cache
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}


sealed trait NodesStorage <: {
  def get(key: NodeHash): Option[NodeEncoded]
  def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesStorage
  def updateCond(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)], inMemory: Boolean): NodesStorage
}

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  *   Key: hash of the RLP encoded node
  *   Value: the RLP encoded node
  */
class NodeStorage(val dataSource: DataSource) extends KeyValueStorage[NodeHash, NodeEncoded, NodeStorage] with NodesStorage {

  val namespace: IndexedSeq[Byte] = Namespaces.NodeNamespace
  def keySerializer: NodeHash => IndexedSeq[Byte] = _.toIndexedSeq
  def valueSerializer: NodeEncoded => IndexedSeq[Byte] = _.toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => NodeEncoded = _.toArray

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)

  def updateCond(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)], inMemory: Boolean): NodesStorage = {
    update(toRemove, toUpsert)
  }
}

class CachedNodeStorage(val storage: NodeStorage, val cache: Cache[NodeHash, NodeEncoded])
  extends CachedKeyValueStorage[NodeHash, NodeEncoded, CachedNodeStorage] with NodesStorage {
  override type I = NodeStorage
  override def apply(cache: Cache[NodeHash, NodeEncoded], storage: NodeStorage): CachedNodeStorage = new CachedNodeStorage(storage, cache)
}


object NodeStorage {
  type NodeHash = ByteString
  type NodeEncoded = Array[Byte]
}
