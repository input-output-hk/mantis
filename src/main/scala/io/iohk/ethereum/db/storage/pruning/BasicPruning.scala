package io.iohk.ethereum.db.storage.pruning

import io.iohk.ethereum.db.storage.{NodeStorage, ReferenceCountNodeStorage}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Logger

/**
  * Basic Pruning mechanism based on counting reference nodes by block number (see ReferenceCountNodeStorage)
  * @param history
  */
case class BasicPruning(history: Int) extends PruningMode with RangePrune with Logger {
  override def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = {
    val from = lastPruned + 1
    val to = from.max(bestBlockNumber - history)
    pruneBetween(from, to, bn => ReferenceCountNodeStorage.prune(bn, nodeStorage))
  }

  override def nodesKeyValueStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt]): NodesKeyValueStorage =
    new ReferenceCountNodeStorage(nodeStorage, blockNumber)
}
