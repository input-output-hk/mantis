package io.iohk.ethereum.db.storage.pruning

import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}
import io.iohk.ethereum.mpt.NodesKeyValueStorage

/**
  * Noop PruningMode. It won't do any pruning and keep a full underlying database
  */
case object ArchivePruning extends PruningMode {
  override def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult =
    PruneResult(0, 0)

  override def nodesKeyValueStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt]): NodesKeyValueStorage =
    new ArchiveNodeStorage(nodeStorage)
}
