package io.iohk.ethereum.db

import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

object NodeStorage {
  type NodeHash = Array[Byte]
  type NodeStorage = KeyValueStorage[NodeHash, MptNode]

  def apply(dataSource: DataSource): NodeStorage = new NodeStorage(
    dataSource = dataSource,
    keySerializer = identity,
    valueSerializer = rlpEncode(_),
    valueDeserializer = rlpDecode[MptNode]
  )

  def put(node: MptNode, nodeStorage: NodeStorage): NodeStorage = nodeStorage.put(key = node.hash, value = node)
}
