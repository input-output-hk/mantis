package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, BlockHeader}
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.duration.{FiniteDuration, _}

trait SyncSpec {

  def obtainSyncConfig(checkingForNewBlockInterval: FiniteDuration = 1.second): SyncConfig = new SyncConfig {
    override val printStatusInterval: FiniteDuration = 1.hour
    override val persistStateSnapshotInterval: FiniteDuration = 20.seconds
    override val targetBlockOffset: Int = 500
    override val branchResolutionBatchSize: Int = 20
    override val blacklistDuration: FiniteDuration = 5.seconds
    override val syncRetryInterval: FiniteDuration = 1.second
    override val checkForNewBlockInterval: FiniteDuration = checkingForNewBlockInterval
    override val startRetryInterval: FiniteDuration = 500.milliseconds
    override val branchResolutionMaxRequests: Int = 100
    override val blockChainOnlyPeersPoolSize: Int = 100
    override val maxConcurrentRequests: Int = 10
    override val blockHeadersPerRequest: Int = 10
    override val blockBodiesPerRequest: Int = 10
    override val doFastSync: Boolean = true
    override val nodesPerRequest: Int = 10
    override val receiptsPerRequest: Int = 10
    override val minPeersToChooseTargetBlock: Int = 2
    override val peerResponseTimeout: FiniteDuration = 1.second
    override val peersScanInterval: FiniteDuration = 500.milliseconds
    override val fastSyncThrottle: FiniteDuration = 100.milliseconds
    val maxQueuedBlockNumberAhead: Int = 10
    val maxQueuedBlockNumberBehind: Int = 10
    val maxNewBlockHashAge: Int = 20
    val maxNewHashes: Int = 64
  }

  val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
  val baseBlockHeader = BlockHeader(
    parentHash = ByteString("unused"),
    ommersHash = ByteString("unused"),
    beneficiary = ByteString("unused"),
    stateRoot = EmptyTrieRootHash,
    transactionsRoot = EmptyTrieRootHash,
    receiptsRoot = EmptyTrieRootHash,
    logsBloom = BloomFilter.EmptyBloomFilter,
    difficulty = 0,
    number = 0,
    gasLimit = 0,
    gasUsed = 0,
    unixTimestamp = 0,
    extraData = ByteString("unused"),
    mixHash = ByteString("unused"),
    nonce = ByteString("unused"))
}
