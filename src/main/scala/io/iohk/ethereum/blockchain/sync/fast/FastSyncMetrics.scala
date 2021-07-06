package io.iohk.ethereum.blockchain.sync.fast

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.MILLISECONDS

import com.google.common.util.concurrent.AtomicDouble

import io.iohk.ethereum.blockchain.sync.fast.FastSync.SyncState
import io.iohk.ethereum.metrics.MetricsContainer

object FastSyncMetrics extends MetricsContainer {

  final private val PivotBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.pivotBlock.number.gauge", new AtomicDouble(0d))
  final private val BestFullBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.bestFullBlock.number.gauge", new AtomicDouble(0d))
  final private val BestHeaderNumberGauge =
    metrics.registry.gauge("fastsync.block.bestHeader.number.gauge", new AtomicDouble(0d))

  final private val MptStateTotalNodesGauge =
    metrics.registry.gauge("fastsync.state.totalNodes.gauge", new AtomicLong(0L))
  final private val MptStateDownloadedNodesGauge =
    metrics.registry.gauge("fastsync.state.downloadedNodes.gauge", new AtomicLong(0L))

  final private val FastSyncTotalTimeMinutesGauge =
    metrics.registry.gauge("fastsync.totaltime.minutes.gauge", new AtomicDouble(0d))

  final private val BlockHeadersDownloadedTimer =
    metrics.registry.timer("fastsync.block.downloadBlockHeaders.timer")
  final private val BlockBodiesDownloadTimer =
    metrics.registry.timer("fastsync.block.downloadBlockBodies.timer")
  final private val BlockReceiptsDownloadTimer =
    metrics.registry.timer("fastsync.block.downloadBlockReceipts.timer")

  final private val MptStateDownloadTimer =
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
