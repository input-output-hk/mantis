package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

class NodeStorage(val dataSource: DataSource) extends KeyValueStorage[ByteString, MptNode] {
  type T = NodeStorage
  override def keySerializer: ByteString => Array[Byte] = _.toArray
  override def valueSerializer: MptNode => Array[Byte] = rlpEncode(_)
  override def valueDeserializer: Array[Byte] => MptNode = rlpDecode[MptNode]

  def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)

  def put(node: MptNode): NodeStorage = put(node.hash, node)
}
