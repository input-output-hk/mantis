package io.iohk.ethereum.db.storage

import io.iohk.ethereum.mpt.NodesKeyValueStorage

package object pruning {

  trait PruningSupport {
    /**
      * Determines and prunes mpt nodes based on last pruned block number tag and the current best block number
      *
      * @param lastPruned      Last pruned block number tag
      * @param bestBlockNumber Current best block number
      * @return PruneResult
      */
    def prune(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult
  }

  trait PruningNodesKeyValueStorage extends NodesKeyValueStorage with PruningSupport

  sealed trait PruningMode
  case object ArchivePruning extends PruningMode
  case class BasicPruning(history: Int) extends PruningMode

  object PruningMode {
    type PruneFn = (=> BigInt, => BigInt) => PruneResult

    /**
      * Create a NodesKeyValueStorage to be used within MerklePatriciaTrie
      *
      * @param blockNumber block number to be used as tag when doing update / removal operations. None can be sent if read only
      * @return Storage to be used
      */
    def nodesKeyValueStorage(pruningMode: PruningMode, nodeStorage: NodeStorage)(blockNumber: Option[BigInt]): PruningNodesKeyValueStorage =
      pruningMode match {
        case ArchivePruning => new ArchiveNodeStorage(nodeStorage)
        case BasicPruning(history) => new ReferenceCountNodeStorage(nodeStorage, history, blockNumber) //FIXME
      }

  }

  case class PruneResult(lastPrunedBlockNumber: BigInt, pruned: Int)

}
