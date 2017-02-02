package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

class MptNodeStorage(val dataSource: DataSource) extends KeyValueStorage[ByteString, MptNode] {
  type T = MptNodeStorage
  override val namespace: Byte = Namespaces.NodeNamespace
  override def keySerializer: ByteString => Array[Byte] = _.toArray
  override def valueSerializer: MptNode => Array[Byte] = rlpEncode(_)
  override def valueDeserializer: Array[Byte] => MptNode = rlpDecode[MptNode]

  def apply(dataSource: DataSource): MptNodeStorage = new MptNodeStorage(dataSource)

  def put(node: MptNode): MptNodeStorage = put(node.hash, node)
}
