package io.iohk.ethereum.db.storage.pruning

import io.iohk.ethereum.utils.Logger

trait RangePrune extends Logger {
  /**
    * Prunes data between [start, end)
    * @param start block to prone
    * @param end block where to stop. This one will not be pruned
    * @param pruneFn function that given a certain block number prunes the data and returns how many nodes were deleted
    * @return resulting PruneResult
    */
  def pruneBetween(start: BigInt, end: BigInt, pruneFn: BigInt => Unit): PruneResult = {
    require(end >= start, "Pruning end should be greater or equal than start")
    log.debug(s"Pruning start for range $start - $end")
    (start until end).foreach(bn => pruneFn(bn))
    val result = PruneResult(end - 1, (end - start).toInt)
    log.debug(s"Pruning finished for range $start - $end. $result.")
    result
  }
}
