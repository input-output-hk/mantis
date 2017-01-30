package io.iohk.ethereum.db

import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

object NodeStorage {
  type NodeStorage = KeyValueStorage[Array[Byte], MptNode]

  def apply(dataSource: DataSource): NodeStorage = new NodeStorage(
    dataSource = dataSource,
    keySerializer = identity,
    valueSerializer = rlpEncode(_),
    valueDeserializer = rlpDecode[MptNode]
  )

  def put(node: MptNode, nodeStorage: NodeStorage): NodeStorage = nodeStorage.update(Seq(), Seq(node.hash -> node))
}
