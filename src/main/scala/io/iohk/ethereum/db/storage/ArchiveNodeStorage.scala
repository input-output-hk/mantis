package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.{BatchOperation, Removal, Upsert}
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{PruneResult, PruningNodesKeyValueStorage}
import io.iohk.ethereum.mpt.NodesKeyValueStorage

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  * Key: hash of the RLP encoded node
  * Value: the RLP encoded node
  */
class ArchiveNodeStorage(nodeStorage: NodeStorage) extends PruningNodesKeyValueStorage {

  override def get(key: NodeHash): Option[NodeEncoded] = nodeStorage.get(key)

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param batchOperations sequence of operations to be applied
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(batchOperations: Seq[BatchOperation[NodeHash, NodeEncoded]]): NodesKeyValueStorage = {
    val upsertOnly = batchOperations.filter {
      case Removal(_) => false
      case Upsert(_, _) => true
    }
    nodeStorage.update(upsertOnly)
    this
  }

  /**
    * Determines and prunes mpt nodes based on last pruned block number tag and the current best block number
    *
    * @param lastPruned      Last pruned block number tag
    * @param bestBlockNumber Current best block number
    * @return PruneResult
    */
  override def prune(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = PruneResult(0, 0)
}
