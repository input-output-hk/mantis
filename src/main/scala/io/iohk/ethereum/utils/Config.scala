package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.rpc.RpcServerConfig
import org.spongycastle.util.encoders.Hex

import scala.collection.JavaConverters._
import scala.concurrent.duration._

object Config {

  private val config = ConfigFactory.load().getConfig("etc-client")

  val clientId: String = config.getString("client-id")

  object Network {
    private val networkConfig = config.getConfig("network")

    val networkId: Int = networkConfig.getInt("network-id")

    object Server {
      private val serverConfig = networkConfig.getConfig("server-address")

      val interface: String = serverConfig.getString("interface")
      val port: Int = serverConfig.getInt("port")
      val listenAddress = new InetSocketAddress(interface, port)
    }

    object Discovery {
      private val discoveryConfig = networkConfig.getConfig("discovery")

      val bootstrapNodes = discoveryConfig.getStringList("bootstrap-nodes").asScala.toSet
      val bootstrapNodesScanInterval = discoveryConfig.getDuration("bootstrap-nodes-scan-interval").toMillis.millis
    }

    object Peer {
      private val peerConfig = networkConfig.getConfig("peer")

      val connectRetryDelay = peerConfig.getDuration("connect-retry-delay").toMillis.millis
      val connectMaxRetries = peerConfig.getInt("connect-max-retries")
      val disconnectPoisonPillTimeout = peerConfig.getDuration("disconnect-poison-pill-timeout").toMillis.millis
      val waitForStatusTimeout = peerConfig.getDuration("wait-for-status-timeout").toMillis.millis
      val waitForChainCheckTimeout = peerConfig.getDuration("wait-for-chain-check-timeout").toMillis.millis
    }

    object Rpc extends RpcServerConfig {
      private val rpcConfig = networkConfig.getConfig("rpc")

      val enabled = rpcConfig.getBoolean("enabled")
      val interface = rpcConfig.getString("interface")
      val port = rpcConfig.getInt("port")
    }

  }

  object Blockchain {
    private val blockchainConfig = config.getConfig("blockchain")

    val genesisDifficulty: Long = blockchainConfig.getLong("genesis-difficulty")
    val genesisHash = ByteString(Hex.decode(blockchainConfig.getString("genesis-hash")))

    val daoForkBlockNumber = BigInt(blockchainConfig.getString("dao-fork-block-number"))
    val daoForkBlockTotalDifficulty = BigInt(blockchainConfig.getString("dao-fork-block-total-difficulty"))
    val daoForkBlockHash = ByteString(Hex.decode(blockchainConfig.getString("dao-fork-block-hash")))

    val chainId: Byte = Hex.decode(blockchainConfig.getString("chain-id")).head
  }

  object FastSync {
    private val fastSyncConfig = config.getConfig("fast-sync")

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
