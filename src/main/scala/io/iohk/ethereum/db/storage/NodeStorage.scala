package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage._

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  *   Key: hash of the RLP encoded node
  *   Value: the RLP encoded node
  */
class NodeStorage(val dataSource: DataSource) extends KeyValueStorage[NodeHash, NodeEncoded] {
  type T = NodeStorage

  val namespace: IndexedSeq[Byte] = Namespaces.NodeNamespace
  def keySerializer: NodeHash => IndexedSeq[Byte] = _.toIndexedSeq
  def valueSerializer: NodeEncoded => IndexedSeq[Byte] = _.toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => NodeEncoded = _.toArray

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)
}

object NodeStorage {
  type NodeHash = Array[Byte]
  type NodeEncoded = Array[Byte]
}
