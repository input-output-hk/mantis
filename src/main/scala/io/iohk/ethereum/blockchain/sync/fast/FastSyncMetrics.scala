package io.iohk.ethereum.blockchain.sync.fast

import com.google.common.util.concurrent.AtomicDouble
import io.iohk.ethereum.blockchain.sync.fast.FastSync.SyncState
import io.iohk.ethereum.metrics.MetricsContainer

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.MILLISECONDS

object FastSyncMetrics extends MetricsContainer {

  private final val PivotBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.pivotBlock.number.gauge", new AtomicDouble(0d))
  private final val BestFullBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.bestFullBlock.number.gauge", new AtomicDouble(0d))
  private final val BestHeaderNumberGauge =
    metrics.registry.gauge("fastsync.block.bestHeader.number.gauge", new AtomicDouble(0d))

  private final val MptStateTotalNodesGauge =
    metrics.registry.gauge("fastsync.state.totalNodes.gauge", new AtomicLong(0L))
  private final val MptStateDownloadedNodesGauge =
    metrics.registry.gauge("fastsync.state.downloadedNodes.gauge", new AtomicLong(0L))

  private final val FastSyncTotalTimeMinutesGauge =
    metrics.registry.gauge("fastsync.totaltime.minutes.gauge", new AtomicDouble(0d))

  private final val BlockHeadersDownloadedTimer =
    metrics.registry.timer("fastsync.block.downloadBlockHeaders.timer")
  private final val BlockBodiesDownloadTimer =
    metrics.registry.timer("fastsync.block.downloadBlockBodies.timer")
  private final val BlockReceiptsDownloadTimer =
    metrics.registry.timer("fastsync.block.downloadBlockReceipts.timer")

  private final val MptStateDownloadTimer =
    metrics.registry.timer("fastsync.state.downloadState.timer")

  def measure(syncState: SyncState): Unit = {
    PivotBlockNumberGauge.set(syncState.pivotBlock.number.toDouble)
    BestFullBlockNumberGauge.set(syncState.lastFullBlockNumber.toDouble)
    BestHeaderNumberGauge.set(syncState.bestBlockHeaderNumber.toDouble)
    MptStateTotalNodesGauge.set(syncState.totalNodesCount)
    MptStateDownloadedNodesGauge.set(syncState.downloadedNodesCount)
  }

  def setFastSyncTotalTimeGauge(time: Double): Unit = FastSyncTotalTimeMinutesGauge.set(time)

  def setBlockHeadersDownloadTime(time: Long): Unit = BlockHeadersDownloadedTimer.record(time, MILLISECONDS)
  def setBlockBodiesDownloadTime(time: Long): Unit = BlockBodiesDownloadTimer.record(time, MILLISECONDS)
  def setBlockReceiptsDownloadTime(time: Long): Unit = BlockReceiptsDownloadTimer.record(time, MILLISECONDS)

  def setMptStateDownloadTime(time: Long): Unit = MptStateDownloadTimer.record(time, MILLISECONDS)
}
