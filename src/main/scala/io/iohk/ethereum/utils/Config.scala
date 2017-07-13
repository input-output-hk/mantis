package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.vm.UInt256
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._
import scala.util.Try

object Config {

  val config = ConfigFactory.load().getConfig("etc-client")

  val clientId: String = config.getString("client-id")

  val clientVersion: String = config.getString("client-version")

  val keysFile: String = config.getString("keys-file")

  val keyStoreDir: String = config.getString("keystore-dir")

  val shutdownTimeout: Duration = config.getDuration("shutdown-timeout").toMillis.millis

  val secureRandomAlgo: String = config.getString("secure-random-algo")

  object Network {
    private val networkConfig = config.getConfig("network")

    val protocolVersion = networkConfig.getString("protocol-version")

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
      val maxPeers: Int = peerConfig.getInt("max-peers")
      val networkId: Int = peerConfig.getInt("network-id")

      val fastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = peerConfig.getInt("max-blocks-headers-per-message")
        val maxBlocksBodiesPerMessage: Int = peerConfig.getInt("max-blocks-bodies-per-message")
        val maxReceiptsPerMessage: Int = peerConfig.getInt("max-receipts-per-message")
        val maxMptComponentsPerMessage: Int = peerConfig.getInt("max-mpt-components-per-message")
      }
    }

    object Rpc extends JsonRpcHttpServerConfig with JsonRpcConfig {
      private val rpcConfig = networkConfig.getConfig("rpc")

      val enabled = rpcConfig.getBoolean("enabled")
      val interface = rpcConfig.getString("interface")
      val port = rpcConfig.getInt("port")

      val apis = rpcConfig.getString("apis").split(",").map(_.trim.toLowerCase)
    }

  }

  object FastSync {
    private val fastSyncConfig = config.getConfig("fast-sync")

    val doFastSync: Boolean = fastSyncConfig.getBoolean("do-fast-sync")

    val peersScanInterval: FiniteDuration = fastSyncConfig.getDuration("peers-scan-interval").toMillis.millis
    val blacklistDuration: FiniteDuration = fastSyncConfig.getDuration("blacklist-duration").toMillis.millis
    val startRetryInterval: FiniteDuration = fastSyncConfig.getDuration("start-retry-interval").toMillis.millis
    val syncRetryInterval: FiniteDuration = fastSyncConfig.getDuration("sync-retry-interval").toMillis.millis
    val peerResponseTimeout: FiniteDuration = fastSyncConfig.getDuration("peer-response-timeout").toMillis.millis
    val printStatusInterval: FiniteDuration = fastSyncConfig.getDuration("print-status-interval").toMillis.millis

    val maxConcurrentRequests: Int = fastSyncConfig.getInt("max-concurrent-requests")
    val blockHeadersPerRequest: Int = fastSyncConfig.getInt("block-headers-per-request")
    val blockBodiesPerRequest: Int = fastSyncConfig.getInt("block-bodies-per-request")
    val receiptsPerRequest: Int = fastSyncConfig.getInt("receipts-per-request")
    val nodesPerRequest: Int = fastSyncConfig.getInt("nodes-per-request")
    val minPeersToChooseTargetBlock: Int = fastSyncConfig.getInt("min-peers-to-choose-target-block")
    val targetBlockOffset: Int = fastSyncConfig.getInt("target-block-offset")
    val persistStateSnapshotInterval: FiniteDuration =
      fastSyncConfig.getDuration("persist-state-snapshot-interval").toMillis.millis

    val checkForNewBlockInterval: FiniteDuration = fastSyncConfig.getDuration("check-for-new-block-interval").toMillis.millis
    val blockResolveDepth: Int = fastSyncConfig.getInt("block-resolving-depth")
  }

  object Db {

    private val dbConfig = config.getConfig("db")
    private val iodbConfig = dbConfig.getConfig("iodb")
    private val levelDbConfig = dbConfig.getConfig("leveldb")

    object Iodb {
      val path: String = iodbConfig.getString("path")
    }

    object LevelDb extends LevelDbConfig {
      override val createIfMissing: Boolean = levelDbConfig.getBoolean("create-if-missing")
      override val paranoidChecks: Boolean = levelDbConfig.getBoolean("paranoid-checks")
      override val verifyChecksums: Boolean = levelDbConfig.getBoolean("verify-checksums")
      override val cacheSize: Int = levelDbConfig.getInt("cache-size")
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
}

object TxPoolConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): TxPoolConfig = {
    val txPoolConfig = etcClientConfig.getConfig("txPool")

    new TxPoolConfig {
      val txPoolSize: Int = txPoolConfig.getInt("tx-pool-size")
      val pendingTxManagerQueryTimeout: FiniteDuration = txPoolConfig.getDuration("pending-tx-manager-query-timeout").toMillis.millis
    }
  }
}

trait MiningConfig {
  val ommersPoolSize: Int
  val blockCacheSize: Int
  val coinbase: Address
  val poolingServicesTimeout: FiniteDuration
}

object MiningConfig {
  def apply(etcClientConfig: TypesafeConfig): MiningConfig = {
    val miningConfig = etcClientConfig.getConfig("mining")

    new MiningConfig {
      val coinbase: Address = Address(Hex.decode(miningConfig.getString("coinbase")))
      val blockCacheSize: Int = miningConfig.getInt("block-cashe-size")
      val ommersPoolSize: Int = miningConfig.getInt("ommers-pool-size")
      val poolingServicesTimeout: FiniteDuration = miningConfig.getDuration("pooling-services-timeout").toMillis.millis
    }
  }
}

trait BlockchainConfig {
  val frontierBlockNumber: BigInt
  val homesteadBlockNumber: BigInt
  val eip150BlockNumber: BigInt
  val eip160BlockNumber: BigInt
  val difficultyBombPauseBlockNumber: BigInt
  val difficultyBombContinueBlockNumber: BigInt

  val customGenesisFileOpt: Option[String]

  val daoForkBlockNumber: BigInt
  val daoForkBlockTotalDifficulty: BigInt
  val daoForkBlockHash: ByteString
  val accountStartNonce: UInt256

  val chainId: Byte

  val monetaryPolicyConfig: MonetaryPolicyConfig
}

object BlockchainConfig {
  def apply(etcClientConfig: TypesafeConfig): BlockchainConfig = {
    val blockchainConfig = etcClientConfig.getConfig("blockchain")

    new BlockchainConfig {
      override val frontierBlockNumber: BigInt = BigInt(blockchainConfig.getString("frontier-block-number"))
      override val homesteadBlockNumber: BigInt = BigInt(blockchainConfig.getString("homestead-block-number"))
      override val eip150BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip150-block-number"))
      override val eip160BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip160-block-number"))
      override val difficultyBombPauseBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-pause-block-number"))
      override val difficultyBombContinueBlockNumber: BigInt = BigInt(blockchainConfig.getString("difficulty-bomb-continue-block-number"))

      override val customGenesisFileOpt: Option[String] = Try(blockchainConfig.getString("custom-genesis-file")).toOption

      override val daoForkBlockNumber: BigInt = BigInt(blockchainConfig.getString("dao-fork-block-number"))
      override val daoForkBlockTotalDifficulty: BigInt = BigInt(blockchainConfig.getString("dao-fork-block-total-difficulty"))
      override val daoForkBlockHash: ByteString = ByteString(Hex.decode(blockchainConfig.getString("dao-fork-block-hash")))
      override val accountStartNonce: UInt256 = UInt256(BigInt(blockchainConfig.getString("account-start-nonce")))

      override val chainId: Byte = Hex.decode(blockchainConfig.getString("chain-id")).head

      override val monetaryPolicyConfig = MonetaryPolicyConfig(blockchainConfig.getConfig("monetary-policy"))
    }
  }
}

case class MonetaryPolicyConfig(
  eraDuration: Int,
  rewardRedutionRate: Double,
  firstEraBlockReward: BigInt
)

object MonetaryPolicyConfig {
  def apply(mpConfig: TypesafeConfig): MonetaryPolicyConfig = MonetaryPolicyConfig(
    mpConfig.getInt("era-duration"),
    mpConfig.getDouble("reward-reduction-rate"),
    BigInt(mpConfig.getString("first-era-block-reward"))
  )
}
