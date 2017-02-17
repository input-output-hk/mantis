package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.db.dataSource.LevelDbConfig
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.spongycastle.util.encoders.Hex

import scala.collection.JavaConversions._
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

      val bootstrapNodes = discoveryConfig.getStringList("bootstrap-nodes").toList
      val bootstrapNodesScanInterval = discoveryConfig.getDuration("bootstrap-nodes-scan-interval").toMillis.millis
    }

    object Peer {
      private val peerConfig = networkConfig.getConfig("peer")

      val connectRetryDelay: FiniteDuration = peerConfig.getDuration("connect-retry-delay").toMillis.millis
      val connectMaxRetries: Int = peerConfig.getInt("connect-max-retries")
      val disconnectPoisonPillTimeout: FiniteDuration = peerConfig.getDuration("disconnect-poison-pill-timeout").toMillis.millis
      val waitForStatusTimeout: FiniteDuration = peerConfig.getDuration("wait-for-status-timeout").toMillis.millis
      val waitForChainCheckTimeout: FiniteDuration = peerConfig.getDuration("wait-for-chain-check-timeout").toMillis.millis

      val maxBlocksHeadersPerMessage: Int = peerConfig.getInt("max-blocks-headers-per-message")
      val maxBlocksBodiesPerMessage: Int = peerConfig.getInt("max-blocks-bodies-per-message")
      val maxReceiptsPerMessage: Int = peerConfig.getInt("max-receipts-per-message")
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
    val genesisBlockBody: BlockBody = BlockBody(Seq(), Seq())
  }

  object FastSync{
    private val fastSyncConfig = config.getConfig("fast-sync")

    val BlocksPerMessage: Int = fastSyncConfig.getInt("blocks-per-message")
    val NodesPerRequest: Int = fastSyncConfig.getInt("nodes-per-request")
    val NodeRequestsInterval: FiniteDuration = fastSyncConfig.getDuration("node-requests-interval").toMillis.millis
  }

  object Db {

    private val dbConfig = config.getConfig("db")

    object LevelDb extends LevelDbConfig {
      override val createIfMissing: Boolean = dbConfig.getBoolean("create-if-missing")
      override val paranoidChecks: Boolean = dbConfig.getBoolean("paranoid-checks")
      override val verifyChecksums: Boolean = dbConfig.getBoolean("verify-checksums")
      override val cacheSize: Int = dbConfig.getInt("cache-size")
    }
  }
}
