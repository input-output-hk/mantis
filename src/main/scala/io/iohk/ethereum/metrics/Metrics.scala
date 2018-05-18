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
   * Measures the block number of the last imported block.
   */
  final val LedgerImportBlockNumber = "ledger.import.block.number"
}
