package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.{ByteString, Timeout}
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.db.dataSource.RocksDbConfig
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning, InMemoryPruning, PruningMode}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.utils.VmConfig.VmMode
import ConfigUtils._

import scala.jdk.CollectionConverters._
import scala.concurrent.duration._
import scala.util.Try

object Config {

  val config = ConfigFactory.load().getConfig("mantis")

  val testmode: Boolean = config.getBoolean("testmode")

  val clientId: String =
    VersionInfo.nodeName(ConfigUtils.getOptionalValue(config, _.getString, "client-identity"))

  val clientVersion: String = VersionInfo.nodeName()

  val nodeKeyFile: String = config.getString("node-key-file")

  val shutdownTimeout: Duration = config.getDuration("shutdown-timeout").toMillis.millis

  val secureRandomAlgo: Option[String] =
    if (config.hasPath("secure-random-algo")) Some(config.getString("secure-random-algo"))
    else None

  val blockchains: BlockchainsConfig = BlockchainsConfig(config.getConfig("blockchains"))

  object Network {
    private val networkConfig = config.getConfig("network")

    val protocolVersion = networkConfig.getInt("protocol-version")

    object Server {
      private val serverConfig = networkConfig.getConfig("server-address")

      val interface: String = serverConfig.getString("interface")
      val port: Int = serverConfig.getInt("port")
      val listenAddress = new InetSocketAddress(interface, port)
    }

    val peer = new PeerConfiguration {
      private val peerConfig = networkConfig.getConfig("peer")
      private val blockchainConfig: BlockchainConfig = blockchains.blockchainConfig

      val connectRetryDelay: FiniteDuration = peerConfig.getDuration("connect-retry-delay").toMillis.millis
      val connectMaxRetries: Int = peerConfig.getInt("connect-max-retries")
      val disconnectPoisonPillTimeout: FiniteDuration =
        peerConfig.getDuration("disconnect-poison-pill-timeout").toMillis.millis
      val waitForHelloTimeout: FiniteDuration = peerConfig.getDuration("wait-for-hello-timeout").toMillis.millis
      val waitForStatusTimeout: FiniteDuration = peerConfig.getDuration("wait-for-status-timeout").toMillis.millis
      val waitForChainCheckTimeout: FiniteDuration =
        peerConfig.getDuration("wait-for-chain-check-timeout").toMillis.millis
      val minOutgoingPeers: Int = peerConfig.getInt("min-outgoing-peers")
      val maxOutgoingPeers: Int = peerConfig.getInt("max-outgoing-peers")
      val maxIncomingPeers: Int = peerConfig.getInt("max-incoming-peers")
      val maxPendingPeers: Int = peerConfig.getInt("max-pending-peers")
      val pruneIncomingPeers: Int = peerConfig.getInt("prune-incoming-peers")
      val minPruneAge: FiniteDuration = peerConfig.getDuration("min-prune-age").toMillis.millis
      val networkId: Int = blockchainConfig.networkId

      val rlpxConfiguration = new RLPxConfiguration {
        val waitForHandshakeTimeout: FiniteDuration =
          peerConfig.getDuration("wait-for-handshake-timeout").toMillis.millis
        val waitForTcpAckTimeout: FiniteDuration = peerConfig.getDuration("wait-for-tcp-ack-timeout").toMillis.millis
      }

      val fastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = peerConfig.getInt("max-blocks-headers-per-message")
        val maxBlocksBodiesPerMessage: Int = peerConfig.getInt("max-blocks-bodies-per-message")
        val maxReceiptsPerMessage: Int = peerConfig.getInt("max-receipts-per-message")
        val maxMptComponentsPerMessage: Int = peerConfig.getInt("max-mpt-components-per-message")
      }
      override val updateNodesInitialDelay: FiniteDuration =
        peerConfig.getDuration("update-nodes-initial-delay").toMillis.millis
      override val updateNodesInterval: FiniteDuration = peerConfig.getDuration("update-nodes-interval").toMillis.millis

      val shortBlacklistDuration: FiniteDuration = peerConfig.getDuration("short-blacklist-duration").toMillis.millis
      val longBlacklistDuration: FiniteDuration = peerConfig.getDuration("long-blacklist-duration").toMillis.millis

      val statSlotDuration: FiniteDuration = peerConfig.getDuration("stat-slot-duration").toMillis.millis
      val statSlotCount: Int = peerConfig.getInt("stat-slot-count")
    }

  }

  case class SyncConfig(
      doFastSync: Boolean,
      peersScanInterval: FiniteDuration,
      blacklistDuration: FiniteDuration,
      criticalBlacklistDuration: FiniteDuration,
      startRetryInterval: FiniteDuration,
      syncRetryInterval: FiniteDuration,
      peerResponseTimeout: FiniteDuration,
      printStatusInterval: FiniteDuration,
      maxConcurrentRequests: Int,
      blockHeadersPerRequest: Int,
      blockBodiesPerRequest: Int,
      receiptsPerRequest: Int,
      nodesPerRequest: Int,
      minPeersToChoosePivotBlock: Int,
      peersToChoosePivotBlockMargin: Int,
      pivotBlockOffset: Int,
      persistStateSnapshotInterval: FiniteDuration,
      blocksBatchSize: Int,
      maxFetcherQueueSize: Int,
      checkForNewBlockInterval: FiniteDuration,
      branchResolutionRequestSize: Int,
      blockChainOnlyPeersPoolSize: Int,
      fastSyncThrottle: FiniteDuration,
      maxQueuedBlockNumberAhead: Int,
      maxQueuedBlockNumberBehind: Int,
      maxNewBlockHashAge: Int,
      maxNewHashes: Int,
      redownloadMissingStateNodes: Boolean,
      fastSyncBlockValidationK: Int,
      fastSyncBlockValidationN: Int,
      fastSyncBlockValidationX: Int,
      maxTargetDifference: Int,
      maximumTargetUpdateFailures: Int,
      stateSyncBloomFilterSize: Int,
      stateSyncPersistBatchSize: Int,
      pivotBlockReScheduleInterval: FiniteDuration,
      maxPivotBlockAge: Int
  )

  object SyncConfig {
    def apply(etcClientConfig: TypesafeConfig): SyncConfig = {
      val syncConfig = etcClientConfig.getConfig("sync")
      SyncConfig(
        doFastSync = syncConfig.getBoolean("do-fast-sync"),
        peersScanInterval = syncConfig.getDuration("peers-scan-interval").toMillis.millis,
        blacklistDuration = syncConfig.getDuration("blacklist-duration").toMillis.millis,
        criticalBlacklistDuration = syncConfig.getDuration("critical-blacklist-duration").toMillis.millis,
        startRetryInterval = syncConfig.getDuration("start-retry-interval").toMillis.millis,
        syncRetryInterval = syncConfig.getDuration("sync-retry-interval").toMillis.millis,
        peerResponseTimeout = syncConfig.getDuration("peer-response-timeout").toMillis.millis,
        printStatusInterval = syncConfig.getDuration("print-status-interval").toMillis.millis,
        maxConcurrentRequests = syncConfig.getInt("max-concurrent-requests"),
        blockHeadersPerRequest = syncConfig.getInt("block-headers-per-request"),
        blockBodiesPerRequest = syncConfig.getInt("block-bodies-per-request"),
        receiptsPerRequest = syncConfig.getInt("receipts-per-request"),
        nodesPerRequest = syncConfig.getInt("nodes-per-request"),
        minPeersToChoosePivotBlock = syncConfig.getInt("min-peers-to-choose-pivot-block"),
        peersToChoosePivotBlockMargin = syncConfig.getInt("peers-to-choose-pivot-block-margin"),
        pivotBlockOffset = syncConfig.getInt("pivot-block-offset"),
        persistStateSnapshotInterval = syncConfig.getDuration("persist-state-snapshot-interval").toMillis.millis,
        blocksBatchSize = syncConfig.getInt("blocks-batch-size"),
        maxFetcherQueueSize = syncConfig.getInt("max-fetcher-queue-size"),
        checkForNewBlockInterval = syncConfig.getDuration("check-for-new-block-interval").toMillis.millis,
        branchResolutionRequestSize = syncConfig.getInt("branch-resolution-request-size"),
        blockChainOnlyPeersPoolSize = syncConfig.getInt("fastsync-block-chain-only-peers-pool"),
        fastSyncThrottle = syncConfig.getDuration("fastsync-throttle").toMillis.millis,
        maxQueuedBlockNumberBehind = syncConfig.getInt("max-queued-block-number-behind"),
        maxQueuedBlockNumberAhead = syncConfig.getInt("max-queued-block-number-ahead"),
        maxNewBlockHashAge = syncConfig.getInt("max-new-block-hash-age"),
        maxNewHashes = syncConfig.getInt("max-new-hashes"),
        redownloadMissingStateNodes = syncConfig.getBoolean("redownload-missing-state-nodes"),
        fastSyncBlockValidationK = syncConfig.getInt("fast-sync-block-validation-k"),
        fastSyncBlockValidationN = syncConfig.getInt("fast-sync-block-validation-n"),
        fastSyncBlockValidationX = syncConfig.getInt("fast-sync-block-validation-x"),
        maxTargetDifference = syncConfig.getInt("max-target-difference"),
        maximumTargetUpdateFailures = syncConfig.getInt("maximum-target-update-failures"),
        stateSyncBloomFilterSize = syncConfig.getInt("state-sync-bloom-filter-size"),
        stateSyncPersistBatchSize = syncConfig.getInt("state-sync-persist-batch-size"),
        pivotBlockReScheduleInterval = syncConfig.getDuration("pivot-block-reschedule-interval").toMillis.millis,
        maxPivotBlockAge = syncConfig.getInt("max-pivot-block-age")
      )
    }
  }

  object Db {

    private val dbConfig = config.getConfig("db")
    private val rocksDbConfig = dbConfig.getConfig("rocksdb")

    val dataSource: String = dbConfig.getString("data-source")

    object RocksDb extends RocksDbConfig {
      override val createIfMissing: Boolean = rocksDbConfig.getBoolean("create-if-missing")
      override val paranoidChecks: Boolean = rocksDbConfig.getBoolean("paranoid-checks")
      override val path: String = rocksDbConfig.getString("path")
      override val maxThreads: Int = rocksDbConfig.getInt("max-threads")
      override val maxOpenFiles: Int = rocksDbConfig.getInt("max-open-files")
      override val verifyChecksums: Boolean = rocksDbConfig.getBoolean("verify-checksums")
      override val levelCompaction: Boolean = rocksDbConfig.getBoolean("level-compaction-dynamic-level-bytes")
      override val blockSize: Long = rocksDbConfig.getLong("block-size")
      override val blockCacheSize: Long = rocksDbConfig.getLong("block-cache-size")
    }

  }

  trait NodeCacheConfig {
    val maxSize: Long
    val maxHoldTime: FiniteDuration
  }

  object NodeCacheConfig extends NodeCacheConfig {
    private val cacheConfig = config.getConfig("node-caching")
    override val maxSize: Long = cacheConfig.getInt("max-size")
    override val maxHoldTime: FiniteDuration = cacheConfig.getDuration("max-hold-time").toMillis.millis
  }

  object InMemoryPruningNodeCacheConfig extends NodeCacheConfig {
    private val cacheConfig = config.getConfig("inmemory-pruning-node-caching")
    override val maxSize: Long = cacheConfig.getInt("max-size")
    override val maxHoldTime: FiniteDuration = cacheConfig.getDuration("max-hold-time").toMillis.millis
  }
}

case class AsyncConfig(askTimeout: Timeout)
object AsyncConfig {
  def apply(mantisConfig: TypesafeConfig): AsyncConfig =
    AsyncConfig(mantisConfig.getConfig("async").getDuration("ask-timeout").toMillis.millis)
}

trait KeyStoreConfig {
  val keyStoreDir: String
  val minimalPassphraseLength: Int
  val allowNoPassphrase: Boolean
}

object KeyStoreConfig {
  def apply(etcClientConfig: TypesafeConfig): KeyStoreConfig = {
    val keyStoreConfig = etcClientConfig.getConfig("keyStore")

    new KeyStoreConfig {
      val keyStoreDir: String = keyStoreConfig.getString("keystore-dir")
      val minimalPassphraseLength: Int = keyStoreConfig.getInt("minimal-passphrase-length")
      val allowNoPassphrase: Boolean = keyStoreConfig.getBoolean("allow-no-passphrase")
    }
  }

  def customKeyStoreConfig(path: String): KeyStoreConfig = {
    new KeyStoreConfig {
      val keyStoreDir: String = path
      val minimalPassphraseLength: Int = 7
      val allowNoPassphrase: Boolean = true
    }
  }
}

trait FilterConfig {
  val filterTimeout: FiniteDuration
  val filterManagerQueryTimeout: FiniteDuration
}

object FilterConfig {
  def apply(etcClientConfig: TypesafeConfig): FilterConfig = {
    val filterConfig = etcClientConfig.getConfig("filter")

    new FilterConfig {
      val filterTimeout: FiniteDuration = filterConfig.getDuration("filter-timeout").toMillis.millis
      val filterManagerQueryTimeout: FiniteDuration =
        filterConfig.getDuration("filter-manager-query-timeout").toMillis.millis
    }
  }
}

trait TxPoolConfig {
  val txPoolSize: Int
  val pendingTxManagerQueryTimeout: FiniteDuration
  val transactionTimeout: FiniteDuration
  val getTransactionFromPoolTimeout: FiniteDuration
}

object TxPoolConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): TxPoolConfig = {
    val txPoolConfig = etcClientConfig.getConfig("txPool")

    new TxPoolConfig {
      val txPoolSize: Int = txPoolConfig.getInt("tx-pool-size")
      val pendingTxManagerQueryTimeout: FiniteDuration =
        txPoolConfig.getDuration("pending-tx-manager-query-timeout").toMillis.millis
      val transactionTimeout: FiniteDuration = txPoolConfig.getDuration("transaction-timeout").toMillis.millis
      val getTransactionFromPoolTimeout: FiniteDuration =
        txPoolConfig.getDuration("get-transaction-from-pool-timeout").toMillis.millis
    }
  }
}

trait DaoForkConfig {

  val forkBlockNumber: BigInt
  val forkBlockHash: ByteString
  val blockExtraData: Option[ByteString]
  val range: Int
  val refundContract: Option[Address]
  val drainList: Seq[Address]

  private lazy val extratadaBlockRange = forkBlockNumber until (forkBlockNumber + range)

  def isDaoForkBlock(blockNumber: BigInt): Boolean = forkBlockNumber == blockNumber

  def requiresExtraData(blockNumber: BigInt): Boolean =
    blockExtraData.isDefined && (extratadaBlockRange contains blockNumber)

  def getExtraData(blockNumber: BigInt): Option[ByteString] =
    if (requiresExtraData(blockNumber)) blockExtraData
    else None
}

object DaoForkConfig {
  def apply(daoConfig: TypesafeConfig): DaoForkConfig = {

    val theForkBlockNumber = BigInt(daoConfig.getString("fork-block-number"))

    val theForkBlockHash = ByteString(Hex.decode(daoConfig.getString("fork-block-hash")))

    new DaoForkConfig {
      override val forkBlockNumber: BigInt = theForkBlockNumber
      override val forkBlockHash: ByteString = theForkBlockHash
      override val blockExtraData: Option[ByteString] =
        Try(daoConfig.getString("block-extra-data")).toOption.map(ByteString(_))
      override val range: Int = Try(daoConfig.getInt("block-extra-data-range")).toOption.getOrElse(0)
      override val refundContract: Option[Address] =
        Try(daoConfig.getString("refund-contract-address")).toOption.map(Address(_))
      override val drainList: List[Address] =
        Try(daoConfig.getStringList("drain-list").asScala.toList).toOption.getOrElse(List.empty).map(Address(_))
    }
  }
}

case class BlockchainsConfig(network: String, blockchains: Map[String, BlockchainConfig]) {
  val blockchainConfig: BlockchainConfig = blockchains(network)
}
object BlockchainsConfig {
  private val networkKey = "network"

  def apply(rawConfig: TypesafeConfig): BlockchainsConfig = BlockchainsConfig(
    network = rawConfig.getString(networkKey),
    blockchains = keys(rawConfig)
      .filterNot(_ == networkKey)
      .map(name => name -> BlockchainConfig.fromRawConfig(rawConfig.getConfig(name)))
      .toMap
  )
}

case class MonetaryPolicyConfig(
    eraDuration: Int,
    rewardReductionRate: Double,
    firstEraBlockReward: BigInt,
    firstEraReducedBlockReward: BigInt,
    firstEraConstantinopleReducedBlockReward: BigInt = 0
) {
  require(
    rewardReductionRate >= 0.0 && rewardReductionRate <= 1.0,
    "reward-reduction-rate should be a value in range [0.0, 1.0]"
  )
}

object MonetaryPolicyConfig {
  def apply(mpConfig: TypesafeConfig): MonetaryPolicyConfig = {
    MonetaryPolicyConfig(
      mpConfig.getInt("era-duration"),
      mpConfig.getDouble("reward-reduction-rate"),
      BigInt(mpConfig.getString("first-era-block-reward")),
      BigInt(mpConfig.getString("first-era-reduced-block-reward")),
      BigInt(mpConfig.getString("first-era-constantinople-reduced-block-reward"))
    )
  }
}

trait PruningConfig {
  val mode: PruningMode
}

object PruningConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): PruningConfig = {
    val pruningConfig = etcClientConfig.getConfig("pruning")

    val pruningMode: PruningMode = pruningConfig.getString("mode") match {
      case "basic" => BasicPruning(pruningConfig.getInt("history"))
      case "archive" => ArchivePruning
      case "inmemory" => InMemoryPruning(pruningConfig.getInt("history"))
    }

    new PruningConfig {
      override val mode: PruningMode = pruningMode
    }
  }
}

case class VmConfig(mode: VmMode, externalConfig: Option[VmConfig.ExternalConfig])

object VmConfig {

  sealed trait VmMode
  object VmMode {
    case object Internal extends VmMode
    case object External extends VmMode
  }

  object ExternalConfig {
    val VmTypeIele = "iele"
    val VmTypeKevm = "kevm"
    val VmTypeMantis = "mantis"
    val VmTypeNone = "none"

    val supportedVmTypes = Set(VmTypeIele, VmTypeKevm, VmTypeMantis, VmTypeNone)
  }

  case class ExternalConfig(vmType: String, executablePath: Option[String], host: String, port: Int)

  def apply(mpConfig: TypesafeConfig): VmConfig = {
    def parseExternalConfig(): ExternalConfig = {
      import ExternalConfig._

      val extConf = mpConfig.getConfig("vm.external")
      val vmType = extConf.getString("vm-type").toLowerCase
      require(
        supportedVmTypes.contains(vmType),
        "vm.external.vm-type must be one of: " + supportedVmTypes.mkString(", ")
      )

      ExternalConfig(
        vmType,
        Try(extConf.getString("executable-path")).toOption,
        extConf.getString("host"),
        extConf.getInt("port")
      )
    }

    mpConfig.getString("vm.mode") match {
      case "internal" => VmConfig(VmMode.Internal, None)
      case "external" => VmConfig(VmMode.External, Some(parseExternalConfig()))
      case other => throw new RuntimeException(s"Unknown VM mode: $other. Expected one of: local, external")
    }
  }
}
