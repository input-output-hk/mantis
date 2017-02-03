package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.DataSource

import NodeStorage._

class NodeStorage(val dataSource: DataSource) extends KeyValueStorage[NodeHash, NodeEncoded] {
  type T = NodeStorage

  val namespace: Byte = Namespaces.NodeNamespace
  def keySerializer: NodeHash => Array[Byte] = identity
  def valueSerializer: NodeEncoded => Array[Byte] = identity
  def valueDeserializer: Array[Byte] => NodeEncoded = identity

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)
}

object NodeStorage {
  type NodeHash = Array[Byte]
  type NodeEncoded = Array[Byte]
}
