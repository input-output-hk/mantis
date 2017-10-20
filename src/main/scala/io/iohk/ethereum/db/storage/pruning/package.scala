package io.iohk.ethereum.db.storage

import io.iohk.ethereum.mpt.NodesKeyValueStorage

package object pruning {

  sealed trait PruningMode
  case object ArchivePruning extends PruningMode
  case class BasicPruning(history: Int) extends PruningMode

  object PruningMode {
    /**
      * Create a NodesKeyValueStorage to be used within MerklePatriciaTrie
      *
      * @param blockNumber block number to be used as tag when doing update / removal operations. None can be sent if read only
      * @return Storage to be used
      */
    def nodesKeyValueStorage(pruningMode: PruningMode, nodeStorage: NodeStorage)(blockNumber: Option[BigInt]): NodesKeyValueStorage =
      pruningMode match {
        case ArchivePruning => new ArchiveNodeStorage(nodeStorage)
        case BasicPruning(history) => new ReferenceCountNodeStorage(nodeStorage, blockNumber)
      }

    def prune(pruningMode: PruningMode, blockNumber: BigInt, nodeStorage: NodeStorage): Unit =
      pruningMode match {
        case ArchivePruning => ArchiveNodeStorage.prune(blockNumber)
        case BasicPruning(history) => ReferenceCountNodeStorage.prune(blockNumber - history, nodeStorage)
      }

  }
}
