package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.http.scaladsl.model.headers.{HttpOrigin, HttpOriginRange}
import akka.util.ByteString
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning, PruningMode}
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.JsonRpcServer.JsonRpcServerConfig
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.utils.NumericUtils._
import org.spongycastle.util.encoders.Hex

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

object Config {

  val config = ConfigFactory.load().getConfig("mantis")

  val clientId: String = config.getString("client-id")

  val clientVersion: String = config.getString("client-version")

  val nodeKeyFile: String = config.getString("node-key-file")

  val keyStoreDir: String = config.getString("keystore-dir")

  val shutdownTimeout: Duration = config.getDuration("shutdown-timeout").toMillis.millis

  val secureRandomAlgo: Option[String] =
    if(config.hasPath("secure-random-algo")) Some(config.getString("secure-random-algo"))
    else None

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

    object Rpc extends JsonRpcServerConfig with JsonRpcConfig {
      private val rpcConfig = networkConfig.getConfig("rpc")

      val mode = rpcConfig.getString("mode")

      val enabled = rpcConfig.getBoolean("enabled")
      val interface = rpcConfig.getString("interface")
      val port = rpcConfig.getInt("port")

      val apis = {
        val providedApis = rpcConfig.getString("apis").split(",").map(_.trim.toLowerCase)
        val invalidApis = providedApis.diff(List("web3", "eth", "net", "personal", "daedalus"))
        require(invalidApis.isEmpty, s"Invalid RPC APIs specified: ${invalidApis.mkString(",")}")
        providedApis
      }

      val certificateKeyStorePath: Option[String] = Try(rpcConfig.getString("certificate-keystore-path")).toOption
      val certificateKeyStoreType: Option[String] = Try(rpcConfig.getString("certificate-keystore-type")).toOption
      val certificatePasswordFile: Option[String] = Try(rpcConfig.getString("certificate-password-file")).toOption

      def parseMultipleOrigins(origins: Seq[String]): HttpOriginRange = HttpOriginRange(origins.map(HttpOrigin(_)):_*)
      def parseSingleOrigin(origin: String): HttpOriginRange = origin match {
          case "*" => HttpOriginRange.*
          case s => HttpOriginRange.Default(HttpOrigin(s) :: Nil)
        }

      val corsAllowedOrigins: HttpOriginRange =
        (Try(parseMultipleOrigins(rpcConfig.getStringList("cors-allowed-origins").asScala)) recoverWith {
          case _ => Try(parseSingleOrigin(rpcConfig.getString("cors-allowed-origins")))
        }).get

      val accountTransactionsMaxBlocks = rpcConfig.getInt("account-transactions-max-blocks")
    }

  }

  case class SyncConfig(
    doFastSync: Boolean,

    peersScanInterval: FiniteDuration,
    blacklistDuration: FiniteDuration,
    startRetryInterval: FiniteDuration,
    syncRetryInterval: FiniteDuration,
    peerResponseTimeout: FiniteDuration,
    printStatusInterval: FiniteDuration,

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
        printStatusInterval = syncConfig.getDuration("print-status-interval").toMillis.millis,

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
}

object TxPoolConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): TxPoolConfig = {
    val txPoolConfig = etcClientConfig.getConfig("txPool")

    new TxPoolConfig {
      val txPoolSize: Int = txPoolConfig.getInt("tx-pool-size")
      val pendingTxManagerQueryTimeout: FiniteDuration = txPoolConfig.getDuration("pending-tx-manager-query-timeout").toMillis.millis
      val transactionTimeout: FiniteDuration = txPoolConfig.getDuration("transaction-timeout").toMillis.millis
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


trait BlockchainConfig {
  val frontierBlockNumber: BigInt
  val homesteadBlockNumber: BigInt
  val eip106BlockNumber: BigInt
  val eip150BlockNumber: BigInt
  val eip155BlockNumber: BigInt
  val eip160BlockNumber: BigInt
  val eip161BlockNumber: BigInt
  val maxCodeSize: Option[BigInt]
  val difficultyBombPauseBlockNumber: BigInt
  val difficultyBombContinueBlockNumber: BigInt

  val customGenesisFileOpt: Option[String]

  val daoForkConfig: Option[DaoForkConfig]

  val accountStartNonce: UInt256

  val chainId: Byte

  val monetaryPolicyConfig: MonetaryPolicyConfig

  val gasTieBreaker: Boolean
}


object BlockchainConfig {

  def apply(etcClientConfig: TypesafeConfig): BlockchainConfig = {
    val blockchainConfig = etcClientConfig.getConfig("blockchain")

    new BlockchainConfig {
      override val frontierBlockNumber: BigInt = BigInt(blockchainConfig.getString("frontier-block-number"))
      override val homesteadBlockNumber: BigInt = BigInt(blockchainConfig.getString("homestead-block-number"))
      override val eip106BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip106-block-number"))
      override val eip150BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip150-block-number"))
      override val eip155BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip155-block-number"))
      override val eip160BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip160-block-number"))
      override val eip161BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip161-block-number"))
      override val maxCodeSize: Option[BigInt] = Try(BigInt(blockchainConfig.getString("max-code-size"))).toOption
      override val difficultyBombPauseBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-pause-block-number"))
      override val difficultyBombContinueBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-continue-block-number"))

      override val customGenesisFileOpt: Option[String] = Try(blockchainConfig.getString("custom-genesis-file")).toOption

      override val daoForkConfig = Try(blockchainConfig.getConfig("dao")).toOption.map(DaoForkConfig(_))
      override val accountStartNonce: UInt256 = UInt256(BigInt(blockchainConfig.getString("account-start-nonce")))

      override val chainId: Byte = {
        val s = blockchainConfig.getString("chain-id")
        val n = parseHexOrDecNumber(s)
        require(n >= 0 && n <= 127, "chain-id must be a number in range [0, 127]")
        n.toByte
      }

      override val monetaryPolicyConfig = MonetaryPolicyConfig(blockchainConfig.getConfig("monetary-policy"))

      val gasTieBreaker: Boolean = blockchainConfig.getBoolean("gas-tie-breaker")
    }
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
