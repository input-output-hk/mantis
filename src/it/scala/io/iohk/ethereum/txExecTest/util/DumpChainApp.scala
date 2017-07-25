package io.iohk.ethereum.txExecTest.util

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Blockchain, _}
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.messages.{PV62, PV63}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{ForkResolver, PeerEventBusActor, PeerManagerActor}
import io.iohk.ethereum.nodebuilder.{AuthHandshakerBuilder, NodeKeyBuilder, SecureRandomBuilder}
import io.iohk.ethereum.utils.{BlockchainConfig, Config, NodeStatus, ServerStatus}
import io.iohk.ethereum.vm.UInt256
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object DumpChainApp extends App with NodeKeyBuilder with SecureRandomBuilder with AuthHandshakerBuilder {
    val conf = ConfigFactory.load("txExecTest/chainDump.conf")
    val node = conf.getString("node")
    val genesisHash = ByteString(Hex.decode(conf.getString("genesisHash")))
    val privateNetworkId = conf.getInt("networkId")
    val startBlock = conf.getInt("startBlock")
    val maxBlocks = conf.getInt("maxBlocks")

    val blockchainConfig = BlockchainConfig(Config.config)

    val peerConfig = new PeerConfiguration {
      override val rlpxConfiguration: RLPxConfiguration = Config.Network.peer.rlpxConfiguration
      override val connectRetryDelay: FiniteDuration = Config.Network.peer.connectRetryDelay
      override val connectMaxRetries: Int = Config.Network.peer.connectMaxRetries
      override val disconnectPoisonPillTimeout: FiniteDuration = Config.Network.peer.disconnectPoisonPillTimeout
      override val waitForHelloTimeout: FiniteDuration = Config.Network.peer.waitForHelloTimeout
      override val waitForStatusTimeout: FiniteDuration = Config.Network.peer.waitForStatusTimeout
      override val waitForChainCheckTimeout: FiniteDuration = Config.Network.peer.waitForChainCheckTimeout
      override val fastSyncHostConfiguration: PeerManagerActor.FastSyncHostConfiguration = Config.Network.peer.fastSyncHostConfiguration
      override val maxPeers: Int = Config.Network.peer.maxPeers
      override val maxIncomingPeers: Int = Config.Network.peer.maxIncomingPeers
      override val networkId: Int = privateNetworkId
      override val updateNodesInitialDelay: FiniteDuration = 5.seconds
      override val updateNodesInterval: FiniteDuration = 20.seconds
    }

    val actorSystem = ActorSystem("etc-client_system")
    trait PruningConfig extends PruningModeComponent {
      override val pruningMode: PruningMode = ArchivePruning
    }
    val storagesInstance = new SharedLevelDBDataSources with PruningConfig with Storages.DefaultStorages

    val blockchain: Blockchain = new BlockchainMock(genesisHash)

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening,
        discoveryStatus = ServerStatus.NotListening)

    lazy val nodeStatusHolder = Agent(nodeStatus)

    lazy val forkResolverOpt =
      if (blockchainConfig.customGenesisFileOpt.isDefined) None
      else Some(new ForkResolver.EtcForkResolver(blockchainConfig))

    private val handshakerConfiguration: EtcHandshakerConfiguration =
      new EtcHandshakerConfiguration {
        override val forkResolverOpt: Option[ForkResolver] = DumpChainApp.forkResolverOpt
        override val nodeStatusHolder: Agent[NodeStatus] = DumpChainApp.nodeStatusHolder
        override val peerConfiguration: PeerConfiguration = peerConfig
        override val blockchain: Blockchain = DumpChainApp.blockchain
        override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
      }

    lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)

    val peerMessageBus = actorSystem.actorOf(PeerEventBusActor.props)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(
      peerDiscoveryManager = actorSystem.deadLetters, // TODO: fixme
      nodeStatusHolder = nodeStatusHolder,
      peerConfiguration = peerConfig,
      peerMessageBus = peerMessageBus,
      knownNodesManager = actorSystem.deadLetters, // TODO: fixme
      handshaker = handshaker,
      authHandshaker = authHandshaker,
      messageDecoder = EthereumMessageDecoder), "peer-manager")
    actorSystem.actorOf(DumpChainActor.props(peerManager,peerMessageBus,startBlock,maxBlocks), "dumper")
  }

  class BlockchainMock(genesisHash: ByteString) extends Blockchain {

    class FakeHeader() extends BlockHeader(ByteString.empty, ByteString.empty, ByteString.empty, ByteString.empty,
      ByteString.empty, ByteString.empty, ByteString.empty, 0, 0, 0, 0, 0, ByteString.empty, ByteString.empty, ByteString.empty) {
      override lazy val hash: ByteString = genesisHash
    }

    override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] = Some(genesisHash)

    override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] = Some(new FakeHeader())

    override def getBlockBodyByHash(hash: ByteString): Option[PV62.BlockBody] = ???

    override def getMptNodeByHash(hash: ByteString): Option[PV63.MptNode] = ???

    override def save(blockHeader: BlockHeader): Unit = ???

    override def save(blockHash: ByteString, blockBody: PV62.BlockBody): Unit = ???

    override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = ???

    override def save(hash: ByteString, evmCode: ByteString): Unit = ???

    override def save(blockhash: ByteString, totalDifficulty: BigInt): Unit = ???

    override def removeBlock(hash: ByteString): Unit = ???

    override def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt] = ???

    override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = ???

    override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = ???

    def getAccount(address: Address, blockNumber: BigInt): Option[Account] = ???

    override def getAccountStorageAt(rootHash: ByteString, position: BigInt): ByteString = ???

    override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] = ???

    override type S = InMemoryWorldStateProxyStorage
    override type WS = InMemoryWorldStateProxy

    override def getWorldStateProxy(blockNumber: BigInt, accountStartNonce: UInt256, stateRootHash: Option[ByteString]): InMemoryWorldStateProxy = ???

    override def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: Option[ByteString]
    ): InMemoryWorldStateProxy = ???
  }
