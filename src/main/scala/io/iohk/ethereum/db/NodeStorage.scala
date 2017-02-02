package io.iohk.ethereum.db

class NodeStorage(val dataSource: DataSource) extends KeyValueStorage[Array[Byte], Array[Byte]] {
  type T = NodeStorage

  override val namespace: Byte = Namespaces.NodeNamespace
  override def keySerializer: Array[Byte] => Array[Byte] = identity
  override def valueSerializer: Array[Byte] => Array[Byte] = identity
  override def valueDeserializer: Array[Byte] => Array[Byte] = identity

  def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)
}
