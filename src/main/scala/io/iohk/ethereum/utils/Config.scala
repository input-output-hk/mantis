package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.rpc.RpcServerConfig
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.spongycastle.util.encoders.Hex

import scala.collection.JavaConverters._
import scala.concurrent.duration._

object Config {

  private val config = ConfigFactory.load().getConfig("etc-client")

  val clientId: String = config.getString("client-id")

  val keysFile: String = config.getString("keys-file")

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

    val peer = new PeerConfiguration {
      private val peerConfig = networkConfig.getConfig("peer")

      val connectRetryDelay: FiniteDuration = peerConfig.getDuration("connect-retry-delay").toMillis.millis
      val connectMaxRetries: Int = peerConfig.getInt("connect-max-retries")
      val disconnectPoisonPillTimeout: FiniteDuration = peerConfig.getDuration("disconnect-poison-pill-timeout").toMillis.millis
      val waitForStatusTimeout: FiniteDuration = peerConfig.getDuration("wait-for-status-timeout").toMillis.millis
      val waitForChainCheckTimeout: FiniteDuration = peerConfig.getDuration("wait-for-chain-check-timeout").toMillis.millis

      val fastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = peerConfig.getInt("max-blocks-headers-per-message")
        val maxBlocksBodiesPerMessage: Int = peerConfig.getInt("max-blocks-bodies-per-message")
        val maxReceiptsPerMessage: Int = peerConfig.getInt("max-receipts-per-message")
        val maxMptComponentsPerMessage: Int = peerConfig.getInt("max-mpt-components-per-message")
      }
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

    val genesisGasLimit = 5000

    val genesisBlockHeader: BlockHeader = BlockHeader(
      parentHash = ByteString(Hex.decode("00" * 32)),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("00" * 20)),
      stateRoot = ByteString(Hex.decode("d7f8974fb5ac78d9ac099b9ad5018bedc2ce0a72dad1827a1709da30580f0544")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00" * 256)),
      difficulty = genesisDifficulty,
      number = 0,
      gasLimit = genesisGasLimit,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString(Hex.decode("11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa")),
      mixHash = ByteString(Hex.decode("00" * 32)),
      nonce = ByteString(Hex.decode("0000000000000042"))
    )

    //this is according to yellow papers but ETH has ~8000 transactions in genesis
    //do we want to ask for them?
    val genesisBlockBody: BlockBody = BlockBody(Seq.empty, Seq.empty)
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
