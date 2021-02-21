package io.iohk.ethereum.db.storage

package object pruning {

  sealed trait PruningMode
  case object ArchivePruning extends PruningMode
  case class BasicPruning(history: Int) extends PruningMode
  case class InMemoryPruning(history: Int) extends PruningMode

  trait PruneSupport {

    /**
      * Remove unused data for the given block number
      * @param blockNumber BlockNumber to prune
      * @param nodeStorage NodeStorage
      */
    def prune(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit

    /**
      * Rollbacks blocknumber changes
      * @param blockNumber BlockNumber to rollback
      * @param nodeStorage NodeStorage
      */
    def rollback(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit
  }
}
