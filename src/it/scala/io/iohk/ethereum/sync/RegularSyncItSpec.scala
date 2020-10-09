package io.iohk.ethereum.sync

import java.net.{InetSocketAddress, ServerSocket}
import java.nio.file.Files
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.blockchain.sync.regular.{BlockBroadcasterActor, RegularSync}
import io.iohk.ethereum.blockchain.sync.{BlockBroadcast, BlockchainHostActor, PeersClient, TestSyncConfig}
import io.iohk.ethereum.consensus.ethash.{EthashConfig, EthashConsensus}
import io.iohk.ethereum.consensus.{ConsensusConfig, FullConsensusConfig, ethash}
import io.iohk.ethereum.db.components.{RocksDbDataSourceComponent, Storages}
import io.iohk.ethereum.db.dataSource.{RocksDbConfig, RocksDbDataSource}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.db.storage.{AppStateStorage, Namespaces}
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess, InMemoryWorldStateProxy, Ledger, LedgerImpl}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.discovery.Node
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.{DiscoveredNodesInfo, DiscoveryNodeInfo}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{EtcPeerManagerActor, ForkResolver, KnownNodesManager, PeerEventBusActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.nodebuilder.{PruningConfigBuilder, SecureRandomBuilder, ShutdownHookBuilder, VmSetup}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.sync.RegularSyncItSpec.{FakePeer, IdentityUpdate}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.{Fixtures, FlatSpecBase, Timeouts}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class RegularSyncItSpec extends FlatSpecBase with Matchers with BeforeAndAfter {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  it should "should sync blockchain with same best block" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      val blockNumer: BigInt = 2000
      for {
        _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer1.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.broadcastBlock()(IdentityUpdate).delayExecution(1.seconds)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer)
      } yield {
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber())
      }
  }

  it should "should sync blockchain progressing forward in the same time" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      val blockNumer: BigInt = 2000
      val blockNumerOnTop: BigInt = blockNumer + 1
      for {
        _ <- peer2.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer1.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.mineNewBlock()(IdentityUpdate).delayExecution(50.milliseconds)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumerOnTop)
      } yield {
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber())
      }
  }

}

object RegularSyncItSpec {

  private def retryUntilWithDelay[A](source: Task[A], delay: FiniteDuration, maxRetries: Int)(
    predicate: A => Boolean
  ): Task[A] = {
    source.delayExecution(delay).flatMap { result =>
      if (predicate(result)) {
        Task.now(result)
      } else {
        if (maxRetries > 0) {
          retryUntilWithDelay(source, delay, maxRetries - 1)(predicate)
        } else {
          Task.raiseError(new TimeoutException("Task time out after all retries"))
        }
      }
    }
  }

  def randomAddress(): InetSocketAddress = {
    val s = new ServerSocket(0)
    try {
      new InetSocketAddress("localhost", s.getLocalPort)
    } finally {
      s.close()
    }
  }

  val IdentityUpdate: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = (_, world) => world

  final case class BlockchainState(bestBlock: Block, currentWorldState: InMemoryWorldStateProxy, currentTd: BigInt)

  class ValidatorsExecutorAlwaysSucceed extends MockValidatorsAlwaysSucceed {
    override def validateBlockAfterExecution(block: Block, stateRootHash: ByteString, receipts: Seq[Receipt], gasUsed: BigInt)
    : Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
  }

  object ValidatorsExecutorAlwaysSucceed extends ValidatorsExecutorAlwaysSucceed

  class FakePeer(peerName: String) extends SecureRandomBuilder with TestSyncConfig {
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

    lazy val tempDir = Files.createTempDirectory("temp-regular-sync")

    def getRockDbTestConfig(dbPath: String): RocksDbConfig = {
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

    val bl: BlockchainImpl = BlockchainImpl(storagesInstance.storages)

    val genesis = Block(
      Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
      Fixtures.Blocks.Genesis.body
    )

    bl.save(genesis, Seq(), genesis.header.difficulty, saveAsBestBlock = true)

    lazy val nh = nodeStatusHolder

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
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

    lazy val peerEventBus: ActorRef = system.actorOf(PeerEventBusActor.props, "peer-event-bus")

    private val handshakerConfiguration: EtcHandshakerConfiguration =
      new EtcHandshakerConfiguration {
        override val forkResolverOpt: Option[ForkResolver] = None
        override val nodeStatusHolder: AtomicReference[NodeStatus] = nh
        override val peerConfiguration: PeerConfiguration = peerConf
        override val blockchain: Blockchain = bl
        override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
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
        EthereumMessageDecoder
      ),
      "peer-manager"
    )

    lazy val etcPeerManager: ActorRef = system.actorOf(
      EtcPeerManagerActor.props(peerManager,
        peerEventBus,
        storagesInstance.storages.appStateStorage,
        None),
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

    val testSyncConfig = syncConfig.copy(
       minPeersToChooseTargetBlock = 1,
       peersScanInterval = 5.milliseconds,
       blockHeadersPerRequest = 200,
       blockBodiesPerRequest = 50,
       receiptsPerRequest = 50,
       fastSyncThrottle = 10.milliseconds,
       startRetryInterval = 50.milliseconds,
       nodesPerRequest = 200,
       maxTargetDifference = 1,
       syncRetryInterval = 50.milliseconds,
       printStatusInterval = 100.milliseconds
    )

    lazy val broadcaster = new BlockBroadcast(etcPeerManager, testSyncConfig)

    lazy val peersClient: ActorRef = system.actorOf(PeersClient.props(etcPeerManager,
      peerEventBus,
      testSyncConfig,
      system.scheduler), "peers-client")

    def buildEthashConsensus(): ethash.EthashConsensus = {
      val consensusConfig: ConsensusConfig = ConsensusConfig(Config.config)(ShutdownHookBuilder)
      val specificConfig: EthashConfig = ethash.EthashConfig(config)
      val fullConfig = FullConsensusConfig(consensusConfig, specificConfig)
      val vm =  VmSetup.vm(VmConfig(config), blockchainConfig, testMode = false)
      val consensus = EthashConsensus(vm, bl, blockchainConfig, fullConfig, ValidatorsExecutorAlwaysSucceed)
      consensus
    }

    lazy val ledger: Ledger = new LedgerImpl(bl, blockchainConfig, syncConfig, buildEthashConsensus, ExecutionContext.global)

    lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(bl,
      1),
      "ommers-pool")

    lazy val pendingTransactionsManager: ActorRef = system.actorOf(
      PendingTransactionsManager.props(TxPoolConfig(config),
        peerManager,
        etcPeerManager,
        peerEventBus),
      "pending-transactions-manager" )

    lazy val broadcasterActor = system.actorOf(
      BlockBroadcasterActor.props(broadcaster, peerEventBus, etcPeerManager, testSyncConfig, system.scheduler)
    )

    lazy val regularSync = system.actorOf(
      RegularSync.props(
        peersClient,
        etcPeerManager,
        peerEventBus,
        ledger,
        bl,
        testSyncConfig,
        ommersPool,
        pendingTransactionsManager,
        system.scheduler
      )
    )

    private def getMptForBlock(block: Block): InMemoryWorldStateProxy = {
      bl.getWorldStateProxy(
        blockNumber = block.number,
        accountStartNonce = blockchainConfig.accountStartNonce,
        stateRootHash = Some(block.header.stateRoot),
        noEmptyAccounts = EvmConfig.forBlock(block.number, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )
    }

    private def broadcastBlock(block: Block, td: BigInt): Unit = {
      broadcasterActor ! BroadcastBlock(NewBlock(block, td))
    }

    def getCurrentState(): BlockchainState = {
      val bestBlock = bl.getBestBlock()
      val currentWorldState = getMptForBlock(bestBlock)
      val currentTd = bl.getTotalDifficultyByHash(bestBlock.hash).get
      BlockchainState(bestBlock, currentWorldState, currentTd)
    }

    def startRegularSync(): Task[Unit] = Task {
      regularSync ! RegularSync.Start
    }

    def mineNewBlock()(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = Task {
      val block: Block = bl.getBestBlock()
      val currentTd = bl.getTotalDifficultyByHash(block.hash).get
      val currentWolrd = getMptForBlock(block)
      val (newBlock, newTd, newWorld) = createChildBlock(block, currentTd, currentWolrd)(updateWorldForBlock)
      regularSync ! RegularSync.MinedBlock(newBlock)
    }

    def waitForRegularSyncLoadLastBlock(blockNumer: BigInt): Task[Boolean] = {
      retryUntilWithDelay(
        Task(bl.getBestBlockNumber() == blockNumer), 1.second,90) { isDone => isDone }
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

    def broadcastBlock()(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = {
      Task(bl.getBestBlock()).flatMap { block => Task {
          val currentTd = bl.getTotalDifficultyByHash(block.hash).get
          val currentWolrd = getMptForBlock(block)
          val (newBlock, newTd, newWorld) = createChildBlock(block, currentTd, currentWolrd)(updateWorldForBlock)
          broadcastBlock(newBlock, newTd)
        }
      }
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
  }

  object FakePeer {

    def startFakePeer(peerName: String): Task[FakePeer] = {
      for {
        peer <- Task(new FakePeer(peerName))
        _ <- peer.startPeer()
      } yield peer
    }

    def start2FakePeersRes(): Resource[Task, (FakePeer, FakePeer)] = {
      Resource.make {
        Task.parZip2(startFakePeer("Peer1"), startFakePeer("Peer2"))
      } { case (peer, peer1) => Task.parMap2(peer.shutdown(), peer1.shutdown())((_, _) => ()) }
    }
  }

  object ShutdownHookBuilder extends ShutdownHookBuilder with Logger
}