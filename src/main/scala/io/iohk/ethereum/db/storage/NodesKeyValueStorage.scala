package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash

/** Storage of serialized nodes, materialized as key-value store */
trait NodesKeyValueStorage extends SimpleMap[NodeHash, NodeEncoded, NodesKeyValueStorage] {
  def persist(): Unit
}
