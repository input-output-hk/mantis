package io.iohk.ethereum.sync

import java.net.{InetSocketAddress, ServerSocket}
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.{BlockchainHostActor, FastSync, TestSyncConfig}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainImpl}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.discovery.Node
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.{DiscoveredNodesInfo, DiscoveryNodeInfo}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{EtcPeerManagerActor, ForkResolver, KnownNodesManager, PeerEventBusActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.nodebuilder.{PruningConfigBuilder, SecureRandomBuilder}
import io.iohk.ethereum.sync.FastSyncItSpec.{FakePeer, customTestCaseResourceM}
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus, VmConfig}
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.{Fixtures, Timeouts}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{Assertion, AsyncFlatSpec, BeforeAndAfter, Matchers}

import scala.concurrent.Future
import scala.concurrent.duration._

class FastSyncItSpec extends AsyncFlatSpec with Matchers with BeforeAndAfter {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  "FastSync" should "should sync blockchain without state nodes" in customTestCaseResourceM(FakePeer.start3FakePeersRes()) {
    case (peer1, peer2, peer3) =>
      for {
        _ <- Task.parZip3(peer1.startPeer(), peer2.startPeer(), peer3.startPeer())
        _ <- peer2.saveNBlocks(1000)
        _ <- peer3.saveNBlocks(1000)
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.startFastSync()
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.syncConfig.targetBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.syncConfig.targetBlockOffset)
      }
  }

}

object FastSyncItSpec {
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

  def customTestCaseResourceM[T](fixture: Resource[Task, T])
                                (theTest: T => Task[Assertion])(implicit s: Scheduler): Future[Assertion] = {
    fixture.use(theTest).runToFuture
  }

  def generateBlockChain(startBlock: Block, number: Int): Seq[Block] = {
    def recur(last: Block, blocksLeft: Int, blocksCreated: List[Block]): List[Block] = {
      if (blocksLeft <= 0) {
        blocksCreated.reverse
      } else {
        val newBlock = last.copy(header = last.header.copy(parentHash = last.header.hash, number = last.header.number + 1))
        recur(newBlock, blocksLeft - 1, newBlock :: blocksCreated)
      }
    }
    recur(startBlock, number, List.empty)
  }

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

    sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
      override lazy val pruningMode: PruningMode = ArchivePruning
    }

    lazy val nodeStatusHolder = new AtomicReference(nodeStatus)
    lazy val storagesInstance = new SharedEphemDataSources with LocalPruningConfigBuilder with Storages.DefaultStorages
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

    val bl = BlockchainImpl(storagesInstance.storages)

    val genesis = Block(Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash) ), Fixtures.Blocks.Genesis.body)

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

    lazy val peerEventBus = system.actorOf(PeerEventBusActor.props, "peer-event-bus")

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

    lazy val peerManager: ActorRef = system.actorOf(PeerManagerActor.props(
      peerDiscoveryManager,
      Config.Network.peer,
      peerEventBus,
      knownNodesManager,
      handshaker,
      authHandshaker,
      EthereumMessageDecoder
    ), "peer-manager")

    lazy val etcPeerManager: ActorRef = system.actorOf(EtcPeerManagerActor.props(
      peerManager, peerEventBus, storagesInstance.storages.appStateStorage, None), "etc-peer-manager")

    val blockchainHost: ActorRef = system.actorOf(BlockchainHostActor.props(
      bl, peerConf, peerEventBus, etcPeerManager), "blockchain-host")

    lazy val server: ActorRef = system.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    val listenAddress = randomAddress()

    lazy val node =
      DiscoveryNodeInfo(Node(ByteString(nodeStatus.nodeId), listenAddress.getAddress, listenAddress.getPort, listenAddress.getPort), 1)

    lazy val vmConfig = VmConfig(Config.config)

    lazy val validators= new MockValidatorsAlwaysSucceed

    val testSyncConfig = syncConfig.copy(
      minPeersToChooseTargetBlock = 1,
      peersScanInterval = 1.second,
      blockHeadersPerRequest = 200,
      blockBodiesPerRequest = 50,
      receiptsPerRequest = 50,
      fastSyncThrottle = 10.milliseconds,
      startRetryInterval = 50.milliseconds,
    )

    lazy val fastSync = system.actorOf(FastSync.props(
      storagesInstance.storages.fastSyncStateStorage,
      storagesInstance.storages.appStateStorage,
      bl,
      validators,
      peerEventBus,
      etcPeerManager,
      testSyncConfig,
      system.scheduler
    ))

    def getMptForBlock(blockHeaderNumber: BigInt) = {
      bl.getWorldStateProxy(
        blockNumber = blockHeaderNumber,
        accountStartNonce = blockchainConfig.accountStartNonce,
        stateRootHash = bl.getBlockByNumber(blockHeaderNumber).map(_.header.stateRoot),
        noEmptyAccounts = EvmConfig.forBlock(blockHeaderNumber, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )
    }

    def startPeer(): Task[Unit] = {
      for {
        _ <- Task {
          peerManager ! PeerManagerActor.StartConnecting
          server ! ServerActor.StartServer(listenAddress)
        }
        _ <- retryUntilWithDelay(Task(nodeStatusHolder.get()), 1.second, 5) {status =>
          status.serverStatus == Listening(listenAddress)
        }
      } yield ()
    }

    def shutdown(): Task[Unit] = {
      Task.deferFuture(system.terminate()).map(_ => ())
    }

    def connectToPeers(nodes: Set[DiscoveryNodeInfo]): Task[Unit] = {
      for {
        _ <- Task {
          peerManager ! DiscoveredNodesInfo(nodes)
        }
        _ <- retryUntilWithDelay(Task(storagesInstance.storages.knownNodesStorage.getKnownNodes()), 1.second, 5){ knownNodes =>
            val requestedNodes = nodes.map(_.node.id)
            val currentNodes = knownNodes.map(Node.fromUri).map(_.id)
            requestedNodes.subsetOf(currentNodes)
        }
      } yield ()
    }

    import akka.pattern.ask
    def getHandshakedPeers: Task[PeerManagerActor.Peers] = {
      Task.deferFutureAction{s =>
        implicit val ec = s
        (peerManager ? PeerManagerActor.GetPeers).mapTo[PeerManagerActor.Peers]
      }
    }

    def saveNBlocks(n: Int) = Task {
      val lastBlock = bl.getBestBlock()
      val chain = generateBlockChain(lastBlock, n)
      chain.foreach(block => bl.save(block, Seq(), block.header.difficulty, true))
    }

    def startFastSync(): Task[Unit] = Task {
      fastSync ! FastSync.Start
    }

    def waitForFastSyncFinish(): Task[Boolean] = {
      retryUntilWithDelay(Task(storagesInstance.storages.appStateStorage.isFastSyncDone()), 1.second, 30){ isDone =>
        isDone
      }
    }
  }

  object FakePeer {
    def startFakePeer(peerName: String): Task[FakePeer] = {
      for {
        peer <- Task(new FakePeer(peerName)).memoizeOnSuccess
        _ <- peer.startPeer()
      } yield peer
    }

    def start1FakePeerRes(): Resource[Task, FakePeer] = {
      Resource.make {
        startFakePeer("Peer1")
      } { peer =>
        peer.shutdown()
      }
    }

    def start2FakePeersRes() = {
      Resource.make {
        Task.parZip2(startFakePeer("Peer1"), startFakePeer("Peer2"))
      } { case (peer, peer1) => Task.parMap2(peer.shutdown(), peer1.shutdown())((_ ,_)=> ())}
    }

    def start3FakePeersRes() = {
      Resource.make {
        Task.parZip3( startFakePeer("Peer1"), startFakePeer("Peer2"),  startFakePeer("Peer3"))
      } { case (peer, peer1, peer2) => Task.parMap3(peer.shutdown(), peer1.shutdown(), peer2.shutdown())((_ ,_, _)=> ())}
    }
  }
}