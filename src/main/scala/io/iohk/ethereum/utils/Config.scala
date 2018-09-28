package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning, PruningMode}
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.utils.NumericUtils._
import io.iohk.ethereum.utils.VmConfig.VmMode
import org.spongycastle.util.encoders.Hex
import java.net.InetAddress

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

case class RiemannConfiguration(
  host: String,
  port: Int,
  batchSize: Int,
  bufferSize: Int,
  autoFlushMilliseconds: Int,
  hostName: String
)

object Config {

  def getOptionalString(config: TypesafeConfig, path: String): Option[String] =
    if(config.hasPath(path)) Some(config.getString(path))
    else None

  def getOptionalConfig(config: TypesafeConfig, path: String): Option[TypesafeConfig] =
    if(config.hasPath(path)) Some(config.getConfig(path))
    else None

  val config = ConfigFactory.load().getConfig("mantis")

  val testmode: Boolean = config.getBoolean("testmode")

  val clientId: String = config.getString("client-id")

  val clientVersion: String = config.getString("client-version")

  val nodeKeyFile: String = config.getString("node-key-file")

  val keyStoreDir: String = config.getString("keystore-dir")

  val shutdownTimeout: Duration = config.getDuration("shutdown-timeout").toMillis.millis

  val secureRandomAlgo: Option[String] = getOptionalString(config, "secure-random-algo")

  val healthIntervalMilliseconds: Long = config.getDuration("health-interval").toMillis()

  val riemann = getOptionalConfig(config, "riemann") map { riemannConfig ⇒
    RiemannConfiguration(
      riemannConfig.getString("host"),
      riemannConfig.getInt("port"),
      riemannConfig.getInt("batch-size"),
      riemannConfig.getInt("buffer-size"),
      riemannConfig.getInt("auto-flush-ms"),
      getOptionalString(riemannConfig, "host-name").getOrElse(InetAddress.getLocalHost().getHostName())
    )
  }

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

      val connectRetryDelay: FiniteDuration = peerConfig.getDuration("connect-retry-delay").toMillis.millis
      val connectMaxRetries: Int = peerConfig.getInt("connect-max-retries")
      val disconnectPoisonPillTimeout: FiniteDuration = peerConfig.getDuration("disconnect-poison-pill-timeout").toMillis.millis
      val waitForHelloTimeout: FiniteDuration = peerConfig.getDuration("wait-for-hello-timeout").toMillis.millis
      val waitForStatusTimeout: FiniteDuration = peerConfig.getDuration("wait-for-status-timeout").toMillis.millis
      val waitForChainCheckTimeout: FiniteDuration = peerConfig.getDuration("wait-for-chain-check-timeout").toMillis.millis
      val maxOutgoingPeers: Int = peerConfig.getInt("max-outgoing-peers")
      val maxIncomingPeers: Int = peerConfig.getInt("max-incoming-peers")
      val maxPendingPeers: Int = peerConfig.getInt("max-pending-peers")
      val networkId: Int = peerConfig.getInt("network-id")

      val rlpxConfiguration = new RLPxConfiguration {
        val waitForHandshakeTimeout: FiniteDuration = peerConfig.getDuration("wait-for-handshake-timeout").toMillis.millis
        val waitForTcpAckTimeout: FiniteDuration = peerConfig.getDuration("wait-for-tcp-ack-timeout").toMillis.millis
      }

      val fastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = peerConfig.getInt("max-blocks-headers-per-message")
        val maxBlocksBodiesPerMessage: Int = peerConfig.getInt("max-blocks-bodies-per-message")
        val maxReceiptsPerMessage: Int = peerConfig.getInt("max-receipts-per-message")
        val maxMptComponentsPerMessage: Int = peerConfig.getInt("max-mpt-components-per-message")
      }
      override val updateNodesInitialDelay: FiniteDuration = peerConfig.getDuration("update-nodes-initial-delay").toMillis.millis
      override val updateNodesInterval: FiniteDuration = peerConfig.getDuration("update-nodes-interval").toMillis.millis
    }

  }

  case class SyncConfig(
    doFastSync: Boolean,

    peersScanInterval: FiniteDuration,
    blacklistDuration: FiniteDuration,
    startRetryInterval: FiniteDuration,
    syncRetryInterval: FiniteDuration,
    peerResponseTimeout: FiniteDuration,
    reportStatusInterval: FiniteDuration,

    maxConcurrentRequests: Int,
    blockHeadersPerRequest: Int,
    blockBodiesPerRequest: Int,
    receiptsPerRequest: Int,
    nodesPerRequest: Int,
    minPeersToChooseTargetBlock: Int,
    targetBlockOffset: Int,
    persistStateSnapshotInterval: FiniteDuration,

    checkForNewBlockInterval: FiniteDuration,
    branchResolutionRequestSize: Int,
    blockChainOnlyPeersPoolSize: Int,
    fastSyncThrottle: FiniteDuration,

    maxQueuedBlockNumberAhead: Int,
    maxQueuedBlockNumberBehind: Int,
    broadcastNewBlockHashes: Boolean,

    maxNewBlockHashAge: Int,
    maxNewHashes: Int,

    redownloadMissingStateNodes: Boolean,

    fastSyncBlockValidationK: Int,
    fastSyncBlockValidationN: Int,
    fastSyncBlockValidationX: Int
  )

  object SyncConfig {
    def apply(etcClientConfig: TypesafeConfig): SyncConfig = {
      val syncConfig = etcClientConfig.getConfig("sync")
      SyncConfig(
        doFastSync = syncConfig.getBoolean("do-fast-sync"),

        peersScanInterval = syncConfig.getDuration("peers-scan-interval").toMillis.millis,
        blacklistDuration = syncConfig.getDuration("blacklist-duration").toMillis.millis,
        startRetryInterval = syncConfig.getDuration("start-retry-interval").toMillis.millis,
        syncRetryInterval = syncConfig.getDuration("sync-retry-interval").toMillis.millis,
        peerResponseTimeout = syncConfig.getDuration("peer-response-timeout").toMillis.millis,
        reportStatusInterval = syncConfig.getDuration("print-status-interval").toMillis.millis,

        maxConcurrentRequests = syncConfig.getInt("max-concurrent-requests"),
        blockHeadersPerRequest = syncConfig.getInt("block-headers-per-request"),
        blockBodiesPerRequest = syncConfig.getInt("block-bodies-per-request"),
        receiptsPerRequest = syncConfig.getInt("receipts-per-request"),
        nodesPerRequest = syncConfig.getInt("nodes-per-request"),
        minPeersToChooseTargetBlock = syncConfig.getInt("min-peers-to-choose-target-block"),
        targetBlockOffset = syncConfig.getInt("target-block-offset"),
        persistStateSnapshotInterval =
          syncConfig.getDuration("persist-state-snapshot-interval").toMillis.millis,

        checkForNewBlockInterval = syncConfig.getDuration("check-for-new-block-interval").toMillis.millis,
        branchResolutionRequestSize = syncConfig.getInt("branch-resolution-request-size"),
        blockChainOnlyPeersPoolSize = syncConfig.getInt("fastsync-block-chain-only-peers-pool"),
        fastSyncThrottle = syncConfig.getDuration("fastsync-throttle").toMillis.millis,

        maxQueuedBlockNumberBehind = syncConfig.getInt("max-queued-block-number-behind"),
        maxQueuedBlockNumberAhead = syncConfig.getInt("max-queued-block-number-ahead"),
        maxNewBlockHashAge = syncConfig.getInt("max-new-block-hash-age"),
        maxNewHashes = syncConfig.getInt("max-new-hashes"),
        broadcastNewBlockHashes = syncConfig.getBoolean("broadcast-new-block-hashes"),

        redownloadMissingStateNodes = syncConfig.getBoolean("redownload-missing-state-nodes"),

        fastSyncBlockValidationK = syncConfig.getInt("fast-sync-block-validation-k"),
        fastSyncBlockValidationN = syncConfig.getInt("fast-sync-block-validation-n"),
        fastSyncBlockValidationX = syncConfig.getInt("fast-sync-block-validation-x")
      )
    }
  }

  object Db {

    private val dbConfig = config.getConfig("db")
    private val iodbConfig = dbConfig.getConfig("iodb")
    private val levelDbConfig = dbConfig.getConfig("leveldb")

    object Iodb  {
      val path: String = iodbConfig.getString("path")
    }

    object LevelDb extends LevelDbConfig {
      override val createIfMissing: Boolean = levelDbConfig.getBoolean("create-if-missing")
      override val paranoidChecks: Boolean = levelDbConfig.getBoolean("paranoid-checks")
      override val verifyChecksums: Boolean = levelDbConfig.getBoolean("verify-checksums")
      override val path: String = levelDbConfig.getString("path")
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
      val filterManagerQueryTimeout: FiniteDuration = filterConfig.getDuration("filter-manager-query-timeout").toMillis.millis
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
      val pendingTxManagerQueryTimeout: FiniteDuration = txPoolConfig.getDuration("pending-tx-manager-query-timeout").toMillis.millis
      val transactionTimeout: FiniteDuration = txPoolConfig.getDuration("transaction-timeout").toMillis.millis
      val getTransactionFromPoolTimeout: FiniteDuration = txPoolConfig.getDuration("get-transaction-from-pool-timeout").toMillis.millis
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

  private lazy val extratadaBlockRange = forkBlockNumber until(forkBlockNumber + range)

  def isDaoForkBlock(blockNumber: BigInt): Boolean = forkBlockNumber == blockNumber

  def requiresExtraData(blockNumber: BigInt): Boolean = blockExtraData.isDefined && (extratadaBlockRange contains blockNumber)

  def getExtraData(blockNumber: BigInt): Option[ByteString] =
    if(requiresExtraData(blockNumber)) blockExtraData
    else None
}

object DaoForkConfig {
  def apply(daoConfig: TypesafeConfig): DaoForkConfig = {

    val theForkBlockNumber = BigInt(daoConfig.getString("fork-block-number"))

    val theForkBlockHash = ByteString(Hex.decode(daoConfig.getString("fork-block-hash")))

    new DaoForkConfig {
      override val forkBlockNumber: BigInt = theForkBlockNumber
      override val forkBlockHash: ByteString = theForkBlockHash
      override val blockExtraData: Option[ByteString] = Try(daoConfig.getString("block-extra-data")).toOption.map(ByteString(_))
      override val range: Int = Try(daoConfig.getInt("block-extra-data-range")).toOption.getOrElse(0)
      override val refundContract: Option[Address] = Try(daoConfig.getString("refund-contract-address")).toOption.map(Address(_))
      override val drainList: List[Address] = Try(daoConfig.getStringList("drain-list").asScala.toList).toOption.getOrElse(List.empty).map(Address(_))
    }
  }
}


case class BlockchainConfig(
  frontierBlockNumber: BigInt,
  homesteadBlockNumber: BigInt,
  eip106BlockNumber: BigInt,
  eip150BlockNumber: BigInt,
  eip155BlockNumber: BigInt,
  eip160BlockNumber: BigInt,
  eip161BlockNumber: BigInt,
  danseBlockNumber: BigInt,
  maxCodeSize: Option[BigInt],
  difficultyBombPauseBlockNumber: BigInt,
  difficultyBombContinueBlockNumber: BigInt,
  customGenesisFileOpt: Option[String],
  daoForkConfig: Option[DaoForkConfig],
  accountStartNonce: UInt256,
  chainId: Byte,
  monetaryPolicyConfig: MonetaryPolicyConfig,
  gasTieBreaker: Boolean,
  ethCompatibilityMode: Boolean,
  constantBlockGasLimit: Option[BigInt]
)


object BlockchainConfig {

  def apply(etcClientConfig: TypesafeConfig): BlockchainConfig = {
    val blockchainConfig = etcClientConfig.getConfig("blockchain")

    BlockchainConfig(
      frontierBlockNumber = BigInt(blockchainConfig.getString("frontier-block-number")),
      homesteadBlockNumber = BigInt(blockchainConfig.getString("homestead-block-number")),
      eip106BlockNumber = BigInt(blockchainConfig.getString("eip106-block-number")),
      eip150BlockNumber = BigInt(blockchainConfig.getString("eip150-block-number")),
      eip155BlockNumber = BigInt(blockchainConfig.getString("eip155-block-number")),
      eip160BlockNumber = BigInt(blockchainConfig.getString("eip160-block-number")),
      eip161BlockNumber = BigInt(blockchainConfig.getString("eip161-block-number")),
      danseBlockNumber = BigInt(blockchainConfig.getString("danse-block-number")),
      maxCodeSize = Try(BigInt(blockchainConfig.getString("max-code-size"))).toOption,
      difficultyBombPauseBlockNumber = BigInt(blockchainConfig.getString("difficulty-bomb-pause-block-number")),
      difficultyBombContinueBlockNumber = BigInt(blockchainConfig.getString("difficulty-bomb-continue-block-number")),

      customGenesisFileOpt = Try(blockchainConfig.getString("custom-genesis-file")).toOption,

      daoForkConfig = Try(blockchainConfig.getConfig("dao")).toOption.map(DaoForkConfig(_)),
      accountStartNonce = UInt256(BigInt(blockchainConfig.getString("account-start-nonce"))),

      chainId = {
        val s = blockchainConfig.getString("chain-id")
        val n = parseHexOrDecNumber(s)
        require(n >= 0 && n <= 127, "chain-id must be a number in range [0, 127]")
        n.toByte
      },

      monetaryPolicyConfig = MonetaryPolicyConfig(blockchainConfig.getConfig("monetary-policy")),

      gasTieBreaker = blockchainConfig.getBoolean("gas-tie-breaker"),

      ethCompatibilityMode = blockchainConfig.getBoolean("eth-compatibility-mode"),

      constantBlockGasLimit =
        Try(blockchainConfig.getString("constant-block-gas-limit"))
          .toOption.map(NumericUtils.parseHexOrDecNumber)
    )
  }
}

case class MonetaryPolicyConfig(
  eraDuration: Int,
  rewardReductionRate: Double,
  firstEraBlockReward: BigInt
) {
  require(rewardReductionRate >= 0.0 && rewardReductionRate <= 1.0,
    "reward-reduction-rate should be a value in range [0.0, 1.0]")
}

object MonetaryPolicyConfig {
  def apply(mpConfig: TypesafeConfig): MonetaryPolicyConfig = {
    MonetaryPolicyConfig(
      mpConfig.getInt("era-duration"),
      mpConfig.getDouble("reward-reduction-rate"),
      BigInt(mpConfig.getString("first-era-block-reward"))
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
    }

    new PruningConfig {
      override val mode: PruningMode = pruningMode
    }
  }
}

case class VmConfig(
    mode: VmMode,
    externalConfig: Option[VmConfig.ExternalConfig])

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

    val supportedVmTypes = Set(VmTypeIele, VmTypeKevm, VmTypeMantis)
  }

  case class ExternalConfig(vmType: String, runVm: Boolean, executablePath: Option[String], host: String, port: Int)

  def apply(mpConfig: TypesafeConfig): VmConfig = {
    def parseExternalConfig(): ExternalConfig = {
      import ExternalConfig._

      val extConf = mpConfig.getConfig("vm.external")
      val vmType = extConf.getString("vm-type").toLowerCase
      require(supportedVmTypes.contains(vmType), "vm.external.vm-type must be one of: " + supportedVmTypes.mkString(", "))

      ExternalConfig(
        vmType = vmType,
        runVm = extConf.getBoolean("run-vm"),
        executablePath = Try(extConf.getString("executable-path")).toOption,
        host = extConf.getString("host"),
        port = extConf.getInt("port"))
    }

    mpConfig.getString("vm.mode") match {
      case "internal" => VmConfig(VmMode.Internal, None)
      case "external" => VmConfig(VmMode.External, Some(parseExternalConfig()))
      case other => throw new RuntimeException(s"Unknown VM mode: $other. Expected one of: local, external")
    }
  }
}
