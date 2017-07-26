package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{PruningNodesKeyValueStorage, PruneResult}
import io.iohk.ethereum.mpt.NodesKeyValueStorage

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  * Key: hash of the RLP encoded node
  * Value: the RLP encoded node
  */
class ArchiveNodeStorage(nodeStorage: NodeStorage) extends PruningNodesKeyValueStorage {

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    nodeStorage.update(Nil, toUpsert)
    this
  }

  override def get(key: NodeHash): Option[NodeEncoded] = nodeStorage.get(key)

  /**
    * Determines and prunes mpt nodes based on last pruned block number tag and the current best block number
    *
    * @param lastPruned      Last pruned block number tag
    * @param bestBlockNumber Current best block number
    * @return PruneResult
    */
  override def prune(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = PruneResult(0, 0)
}
