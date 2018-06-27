package io.iohk.ethereum.consensus.atomixraft

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class AtomixRaftForgerMetrics(metrics: Metrics, lastForgedBlockNumber: () ⇒ Double) extends MetricsContainer {
  final val LastForgedBlockNumber = metrics.gauge("raft.last.forged.block.number", lastForgedBlockNumber)

  /**
   * Counts how many blocks forged by the leader.
   */
  final val LeaderForgedBlocksCounter = metrics.counter("raft.leader.forged.blocks.counter")
}
