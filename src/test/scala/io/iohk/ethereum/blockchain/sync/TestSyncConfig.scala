package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._

import io.iohk.ethereum.nodebuilder.SyncConfigBuilder
import io.iohk.ethereum.utils.Config.SyncConfig

trait TestSyncConfig extends SyncConfigBuilder {
  def defaultSyncConfig: SyncConfig = SyncConfig(
    printStatusInterval = 1.second,
    persistStateSnapshotInterval = 2.seconds,
    pivotBlockOffset = 500,
    branchResolutionRequestSize = 30,
    blacklistDuration = 5.seconds,
    criticalBlacklistDuration = 10.seconds,
    syncRetryInterval = 1.second,
    syncSwitchDelay = 0.5.second,
    checkForNewBlockInterval = 1.milli,
    startRetryInterval = 500.milliseconds,
    blockChainOnlyPeersPoolSize = 100,
    maxConcurrentRequests = 10,
    blockHeadersPerRequest = 2,
    blockBodiesPerRequest = 5,
    blocksBatchSize = 5,
    maxFetcherQueueSize = 100,
    doFastSync = false,
    nodesPerRequest = 10,
    receiptsPerRequest = 10,
    minPeersToChoosePivotBlock = 2,
    peersToChoosePivotBlockMargin = 0,
    peersToFetchFrom = 5,
    peerResponseTimeout = 1.second,
    peersScanInterval = 1.hour,
    fastSyncThrottle = 100.milliseconds,
    maxQueuedBlockNumberAhead = 10,
    maxQueuedBlockNumberBehind = 10,
    maxNewBlockHashAge = 20,
    maxNewHashes = 64,
    redownloadMissingStateNodes = true,
    fastSyncBlockValidationK = 100,
    fastSyncBlockValidationN = 2048,
    fastSyncBlockValidationX = 50,
    maxTargetDifference = 5,
    maximumTargetUpdateFailures = 1,
    stateSyncBloomFilterSize = 1000,
    stateSyncPersistBatchSize = 1000,
    pivotBlockReScheduleInterval = 1.second,
    maxPivotBlockAge = 96,
    fastSyncMaxBatchRetries = 3,
    maxPivotBlockFailuresCount = 3
  )

  override lazy val syncConfig: SyncConfig = defaultSyncConfig
}
