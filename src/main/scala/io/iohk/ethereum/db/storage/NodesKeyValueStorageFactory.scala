package io.iohk.ethereum.db.storage

import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Logger

class NodesKeyValueStorageFactory(pruningMode: PruningMode, nodeStorage: NodeStorage) {

  def create(blockNumber: Option[BigInt] = None): NodesKeyValueStorage =
    pruningMode match {
      case Basic(_) => new ReferenceCountNodeStorage(nodeStorage, blockNumber)
      case Archive | _ => new ArchiveNodeStorage(nodeStorage)
    }

  def create(blockNumber: BigInt): NodesKeyValueStorage = create(Some(blockNumber))

  def create: NodesKeyValueStorage = create(None)
}

sealed trait PruningMode {
  def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult
}

object PruningMode {
  /**
    * A function that given the last pruned block and the best block number, determines and prunes nodes
    */
  type PruneFn = (=> BigInt, => BigInt) => PruneResult
}

trait RangePrune extends Logger {
  def pruneBetween(start: BigInt, end: BigInt, pruneFn: BigInt => Int): PruneResult = {
    log.debug(s"Pruning start for range $start - $end")
    val prunedCount = (start until end).foldLeft(0) { (acc, bn) =>
      acc + pruneFn(bn)
    }
    val result = PruneResult(end - 1, prunedCount)
    log.debug(s"Pruning finished for range $start - $end, result $result")
    result
  }
}

case object Archive extends PruningMode {
  override def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = PruneResult(0, 0)
}

case class Basic(history: Int) extends PruningMode with RangePrune with Logger {
  override def prune(nodeStorage: NodeStorage)(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = {
    val from = lastPruned + 1
    val to = from.max(bestBlockNumber - history)
    pruneBetween(from, to, bn => new ReferenceCountNodeStorage(nodeStorage, Some(bn)).prune())
  }
}

case class PruneResult(lastPrunedBlockNumber: BigInt, pruned: Int)

