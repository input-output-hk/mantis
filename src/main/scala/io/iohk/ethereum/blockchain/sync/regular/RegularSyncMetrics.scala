package io.iohk.ethereum.blockchain.sync.regular

import io.iohk.ethereum.metrics.MetricsContainer

import scala.concurrent.duration.NANOSECONDS

object RegularSyncMetrics extends MetricsContainer {
  final private val blockPropagationTimer = "regularsync.blocks.propagation.timer"

  final val MinedBlockPropagationTimer = metrics.timer(blockPropagationTimer, "blocktype", "MinedBlockPropagation")
  final val CheckpointBlockPropagationTimer =
    metrics.timer(blockPropagationTimer, "blocktype", "CheckpointBlockPropagation")
  final val NewBlockPropagationTimer = metrics.timer(blockPropagationTimer, "blocktype", "NewBlockPropagation")
  final val DefaultBlockPropagationTimer = metrics.timer(blockPropagationTimer, "blocktype", "DefaultBlockPropagation")

  def recordMinedBlockPropagationTimer(nanos: Long): Unit = MinedBlockPropagationTimer.record(nanos, NANOSECONDS)
  def recordImportCheckpointPropagationTimer(nanos: Long): Unit =
    CheckpointBlockPropagationTimer.record(nanos, NANOSECONDS)
  def recordImportNewBlockPropagationTimer(nanos: Long): Unit = NewBlockPropagationTimer.record(nanos, NANOSECONDS)
  def recordDefaultBlockPropagationTimer(nanos: Long): Unit = DefaultBlockPropagationTimer.record(nanos, NANOSECONDS)
}
