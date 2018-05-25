package io.iohk.ethereum.metrics

object Metrics {
  /**
   * Signifies that Mantis has started.
   */
  final val StartEvent = "start.event"

  /**
   * Signifies that Mantis has stopped.
   */
  final val StopEvent = "stop.event"

  /**
   * Measures the block number of the last imported block in the Ledger.
   */
  final val LedgerImportBlockNumber = "ledger.import.block.number"

  /**
   * Total number of transactions imported in the Ledger.
   */
  final val LedgerImportTotalTransactionsNumber = "ledger.import.total.transactions.number"

  /**
   * Counts the rate at which transactions are imported in the Ledger.
   */
  final val LedgerImportTransactionsCounter = "ledger.import.transactions.counter"

  /**
   * Signifies a leadership change. This is emitted from the new leader.
   */
  final val RaftLeaderEvent = "raft.leader.event"

  /**
   * How many times this node became a leader
   */
  final val RaftLeadershipsNumber = "raft.leaderships.number"

  /**
   * How many blocks forged by the leader.
   */
  final val RaftLeaderForgedBlocksNumber = "raft.leader.forged.blocks.number"
}
