package io.iohk.ethereum.consensus.atomixraft

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class AtomixRaftConsensusMetrics(metrics: Metrics) extends MetricsContainer {
  /**
   * Signifies a leadership change. This is emitted from the new leader.
   */
  final val LeaderEvent = metrics.deltaSpike("raft.leader.event")

  /**
   * Counts how many times this node became a leader.
   */
  final val BecomeLeaderCounter = metrics.counter("raft.become.leader.counter")

  /**
   * Counts how many times this node changed role.
   */
  final val ChangeRoleCounter = metrics.counter("raft.change.role.counter")
}
