package io.iohk.ethereum.sync.util

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.blockchain.sync.{BlockBroadcast, BlockchainHostActor, TestSyncConfig}
import io.iohk.ethereum.db.components.{RocksDbDataSourceComponent, Storages}
import io.iohk.ethereum.db.dataSource.{RocksDbConfig, RocksDbDataSource}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.db.storage.{AppStateStorage, Namespaces}
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, Node}
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.{DiscoveredNodesInfo, DiscoveryNodeInfo}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{
  EtcPeerManagerActor,
  ForkResolver,
  KnownNodesManager,
  PeerEventBusActor,
  PeerManagerActor,
  ServerActor
}
import io.iohk.ethereum.nodebuilder.{PruningConfigBuilder, SecureRandomBuilder}
import io.iohk.ethereum.sync.util.SyncCommonItSpec._
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.{Fixtures, Timeouts}
import monix.eval.Task

import scala.concurrent.duration.{FiniteDuration, _}

abstract class CommonFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig)
    extends SecureRandomBuilder
    with TestSyncConfig {
  implicit val akkaTimeout: Timeout = Timeout(5.second)

  val config = Config.config

  import scala.language.postfixOps

  implicit val system = ActorSystem(peerName)

  val peerDiscoveryManager = TestProbe().ref

  val nodeKey = io.iohk.ethereum.crypto.generateKeyPair(secureRandom)

  private val nodeStatus =
    NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )

  lazy val tempDir = Files.createTempDirectory("temp-fast-sync")

  def getRockDbTestConfig(dbPath: String) = {
    new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = false
      override val path: String = dbPath
      override val maxThreads: Int = 1
      override val maxOpenFiles: Int = 32
      override val verifyChecksums: Boolean = false
      override val levelCompaction: Boolean = true
      override val blockSize: Long = 16384
      override val blockCacheSize: Long = 33554432
    }
  }

  sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  lazy val nodeStatusHolder = new AtomicReference(nodeStatus)
  lazy val storagesInstance = new RocksDbDataSourceComponent
    with LocalPruningConfigBuilder
    with Storages.DefaultStorages {
    override lazy val dataSource: RocksDbDataSource =
      RocksDbDataSource(getRockDbTestConfig(tempDir.toAbsolutePath.toString), Namespaces.nsSeq)
  }
  lazy val blockchainConfig = Config.blockchains.blockchainConfig
  lazy val discoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)

  /**
    * Default persist interval is 20s, which is too long for tests. As in all tests we treat peer as connected when
    * it is persisted in storage.
    */
  lazy val knownNodesManagerConfig =
    KnownNodesManager.KnownNodesManagerConfig(config).copy(persistInterval = 1.seconds)

  lazy val knownNodesManager = system.actorOf(
    KnownNodesManager.props(
      knownNodesManagerConfig,
      storagesInstance.storages.knownNodesStorage
    )
  )

  val bl = BlockchainImpl(storagesInstance.storages)

  val genesis = Block(
    Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
    Fixtures.Blocks.Genesis.body
  )

  bl.save(genesis, Seq(), genesis.header.difficulty, saveAsBestBlock = true)

  lazy val nh = nodeStatusHolder

  val peerConf = new PeerConfiguration {
    override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
      val maxBlocksHeadersPerMessage: Int = fakePeerCustomConfig.hostConfig.maxBlocksHeadersPerMessage
      val maxBlocksBodiesPerMessage: Int = fakePeerCustomConfig.hostConfig.maxBlocksBodiesPerMessage
      val maxReceiptsPerMessage: Int = fakePeerCustomConfig.hostConfig.maxReceiptsPerMessage
      val maxMptComponentsPerMessage: Int = fakePeerCustomConfig.hostConfig.maxMptComponentsPerMessage
    }
    override val rlpxConfiguration: RLPxConfiguration = new RLPxConfiguration {
      override val waitForTcpAckTimeout: FiniteDuration = Timeouts.normalTimeout
      override val waitForHandshakeTimeout: FiniteDuration = Timeouts.normalTimeout
    }
    override val waitForHelloTimeout: FiniteDuration = 3 seconds
    override val waitForStatusTimeout: FiniteDuration = 30 seconds
    override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
    override val connectMaxRetries: Int = 3
    override val connectRetryDelay: FiniteDuration = 1 second
    override val disconnectPoisonPillTimeout: FiniteDuration = 3 seconds
    override val maxOutgoingPeers = 10
    override val maxIncomingPeers = 5
    override val maxPendingPeers = 5
    override val networkId: Int = 1

    override val updateNodesInitialDelay: FiniteDuration = 5.seconds
    override val updateNodesInterval: FiniteDuration = 20.seconds
    override val shortBlacklistDuration: FiniteDuration = 1.minute
    override val longBlacklistDuration: FiniteDuration = 3.minutes
  }

  lazy val peerEventBus = system.actorOf(PeerEventBusActor.props, "peer-event-bus")

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = None
      override val nodeStatusHolder: AtomicReference[NodeStatus] = nh
      override val peerConfiguration: PeerConfiguration = peerConf
      override val blockchain: Blockchain = bl
      override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
      override val blockchainConfig = CommonFakePeer.this.blockchainConfig // FIXME: remove in ETCM-280
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)

  lazy val authHandshaker: AuthHandshaker = AuthHandshaker(nodeKey, secureRandom)

  lazy val peerManager: ActorRef = system.actorOf(
    PeerManagerActor.props(
      peerDiscoveryManager,
      Config.Network.peer,
      peerEventBus,
      knownNodesManager,
      handshaker,
      authHandshaker,
      EthereumMessageDecoder,
      discoveryConfig
    ),
    "peer-manager"
  )

  lazy val etcPeerManager: ActorRef = system.actorOf(
    EtcPeerManagerActor.props(peerManager, peerEventBus, storagesInstance.storages.appStateStorage, None),
    "etc-peer-manager"
  )

  val blockchainHost: ActorRef =
    system.actorOf(BlockchainHostActor.props(bl, peerConf, peerEventBus, etcPeerManager), "blockchain-host")

  lazy val server: ActorRef = system.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

  val listenAddress = randomAddress()

  lazy val node =
    DiscoveryNodeInfo(
      Node(ByteString(nodeStatus.nodeId), listenAddress.getAddress, listenAddress.getPort, listenAddress.getPort),
      1
    )

  lazy val vmConfig = VmConfig(Config.config)

  val testSyncConfig = syncConfig.copy(
    minPeersToChoosePivotBlock = 1,
    peersScanInterval = 5.milliseconds,
    blockHeadersPerRequest = 200,
    blockBodiesPerRequest = 50,
    receiptsPerRequest = 50,
    fastSyncThrottle = 10.milliseconds,
    startRetryInterval = 50.milliseconds,
    nodesPerRequest = 200,
    maxTargetDifference = 1,
    syncRetryInterval = 50.milliseconds
  )

  lazy val broadcaster = new BlockBroadcast(etcPeerManager, testSyncConfig)

  lazy val broadcasterActor = system.actorOf(
    BlockBroadcasterActor.props(broadcaster, peerEventBus, etcPeerManager, testSyncConfig, system.scheduler)
  )

  private def getMptForBlock(block: Block) = {
    bl.getWorldStateProxy(
      blockNumber = block.number,
      accountStartNonce = blockchainConfig.accountStartNonce,
      stateRootHash = Some(block.header.stateRoot),
      noEmptyAccounts = EvmConfig.forBlock(block.number, blockchainConfig).noEmptyAccounts,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
    )
  }

  private def broadcastBlock(block: Block, td: BigInt) = {
    broadcasterActor ! BroadcastBlock(NewBlock(block, td))
  }

  def getCurrentState(): BlockchainState = {
    val bestBlock = bl.getBestBlock()
    val currentWorldState = getMptForBlock(bestBlock)
    val currentTd = bl.getTotalDifficultyByHash(bestBlock.hash).get
    BlockchainState(bestBlock, currentWorldState, currentTd)
  }

  def startPeer(): Task[Unit] = {
    for {
      _ <- Task {
        peerManager ! PeerManagerActor.StartConnecting
        server ! ServerActor.StartServer(listenAddress)
      }
      _ <- retryUntilWithDelay(Task(nodeStatusHolder.get()), 1.second, 5) { status =>
        status.serverStatus == Listening(listenAddress)
      }
    } yield ()
  }

  def shutdown(): Task[Unit] = {
    for {
      _ <- Task.deferFuture(system.terminate())
      _ <- Task(storagesInstance.dataSource.destroy())
    } yield ()
  }

  def connectToPeers(nodes: Set[DiscoveryNodeInfo]): Task[Unit] = {
    for {
      _ <- Task {
        peerManager ! DiscoveredNodesInfo(nodes)
      }
      _ <- retryUntilWithDelay(Task(storagesInstance.storages.knownNodesStorage.getKnownNodes()), 1.second, 5) {
        knownNodes =>
          val requestedNodes = nodes.map(_.node.id)
          val currentNodes = knownNodes.map(Node.fromUri).map(_.id)
          requestedNodes.subsetOf(currentNodes)
      }
    } yield ()
  }

  private def createChildBlock(parent: Block, parentTd: BigInt, parentWorld: InMemoryWorldStateProxy)(
      updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
  ): (Block, BigInt, InMemoryWorldStateProxy) = {
    val newBlockNumber = parent.header.number + 1
    val newWorld = updateWorldForBlock(newBlockNumber, parentWorld)
    val newBlock = parent.copy(header =
      parent.header.copy(parentHash = parent.header.hash, number = newBlockNumber, stateRoot = newWorld.stateRootHash)
    )
    val newTd = newBlock.header.difficulty + parentTd
    (newBlock, newTd, parentWorld)
  }

  def importBlocksUntil(
      n: BigInt
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = {
    Task(bl.getBestBlock()).flatMap { block =>
      if (block.number >= n) {
        Task(())
      } else {
        Task {
          val currentTd = bl.getTotalDifficultyByHash(block.hash).get
          val currentWolrd = getMptForBlock(block)
          val (newBlock, newTd, newWorld) = createChildBlock(block, currentTd, currentWolrd)(updateWorldForBlock)
          bl.save(newBlock, Seq(), newTd, saveAsBestBlock = true)
          bl.persistCachedNodes()
          broadcastBlock(newBlock, newTd)
        }.flatMap(_ => importBlocksUntil(n)(updateWorldForBlock))
      }
    }
  }

}
