package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  * Key: hash of the RLP encoded node
  * Value: the RLP encoded node
  */
class ArchiveNodeStorage(nodeStorage: NodeStorage) extends NodesKeyValueStorage {

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    nodeStorage.update(Nil, toUpsert)
    this
  }

  override def get(key: NodeHash): Option[NodeEncoded] = nodeStorage.get(key)
}
