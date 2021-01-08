package io.iohk.ethereum.blockchain.sync.fast

import java.util.concurrent.atomic.AtomicLong

import com.google.common.util.concurrent.AtomicDouble
import io.iohk.ethereum.metrics.MetricsContainer

object SyncMetrics extends MetricsContainer {

  private[this] final val PivotBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.pivotBlock.number.gauge", new AtomicDouble(0d))
  private[this] final val BestFullBlockNumberGauge =
    metrics.registry.gauge("fastsync.block.bestFullBlock.number.gauge", new AtomicDouble(0d))
  private[this] final val BestHeaderNumberGauge =
    metrics.registry.gauge("fastsync.block.bestHeader.number.gauge", new AtomicDouble(0d))

  private[this] final val MptStateTotalNodesGauge =
    metrics.registry.gauge("fastsync.state.totalNodes.gauge", new AtomicLong(0L))
  private[this] final val MptStateDownloadedNodesGauge =
    metrics.registry.gauge("fastsync.state.downloadedNodes.gauge", new AtomicLong(0L))

  def measure(syncState: PersistentSyncState): Unit = {
    PivotBlockNumberGauge.set(syncState.pivotBlock.number.toDouble)
    BestFullBlockNumberGauge.set(syncState.lastFullBlockNumber.toDouble)
    BestHeaderNumberGauge.set(syncState.bestBlockHeaderNumber.toDouble)
    MptStateTotalNodesGauge.set(syncState.totalNodesCount)
    MptStateDownloadedNodesGauge.set(syncState.downloadedNodesCount)
  }
}
