package io.iohk.ethereum.db.storage

import io.iohk.ethereum.mpt.NodesKeyValueStorage

package object pruning {

  trait PruningMode {
    /**
      * Determines and prunes mpt nodes based on last pruned block number tag and the current best block number
      * @param nodeStorage Storage to access mpt nodes
      * @param lastPruned Last pruned block number tag
      * @param bestBlockNumber Current best block number
      * @return PruneResult
      */
    def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult

    /**
      * Create a NodesKeyValueStorage to be used within MerklePatriciaTrie
      * @param nodeStorage mpt nodes storage
      * @param blockNumber block number to be used as tag when doing update / removal operations. None can be sent if read only
      * @return Storage to be used
      */
    def nodesKeyValueStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt]): NodesKeyValueStorage
  }

  object PruningMode {
    type PruneFn = (=> BigInt, => BigInt) => PruneResult
  }

  case class PruneResult(lastPrunedBlockNumber: BigInt, pruned: Int)

}
