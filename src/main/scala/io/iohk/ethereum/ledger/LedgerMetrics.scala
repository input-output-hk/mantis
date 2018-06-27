package io.iohk.ethereum.ledger

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class LedgerMetrics(metrics: Metrics, getBestBlockNumber: () ⇒ Double) extends MetricsContainer {
  /**
   * Gauge for the best block number.
   * Note: initially we were actually counting the blocks imported but now we just query the blockchain
   * best number that this node knows about. For historical reasons, and in order to not break existing
   * deployments, we keep a name that refers to `ledger`.
   */
  final val ImportBlockNumber = metrics.gauge("ledger.import.block.number", getBestBlockNumber)

  // Note ... and this is the name we want to migrate to.
  // Note When this happens, the below metric should probably move to another `MetricsContainer`
  final val BestBlockNumber = metrics.gauge("best.block.number", getBestBlockNumber)

  /**
   * Counter for the imported blocks, since last reboot.
   */
  final val ImportedBlocksCounter = metrics.counter("ledger.imported.blocks.counter")

  /**
   * Counter for the imported transactions, since last reboot.
   */
  final val ImportedTransactionsCounter = metrics.counter("ledger.imported.transactions.counter")
}
