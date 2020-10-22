package io.iohk.ethereum.sync

import java.nio.file.Files
import java.util.concurrent.{ThreadLocalRandom, TimeoutException}
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.{Fixtures, Timeouts}
import io.iohk.ethereum.blockchain.sync.{BlockBroadcast, BlockchainHostActor, FastSync, TestSyncConfig}
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.components.{RocksDbDataSourceComponent, Storages}
import io.iohk.ethereum.db.dataSource.{RocksDbConfig, RocksDbDataSource}
import io.iohk.ethereum.db.storage.{AppStateStorage, Namespaces}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Address, Block, Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.{HashNode, MerklePatriciaTrie, MptNode, MptTraversals}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.{
  EtcPeerManagerActor,
  ForkResolver,
  KnownNodesManager,
  PeerEventBusActor,
  PeerManagerActor,
  ServerActor
}
import io.iohk.ethereum.network.discovery.Node
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.{DiscoveredNodesInfo, DiscoveryNodeInfo}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.nodebuilder.{PruningConfigBuilder, SecureRandomBuilder}
import io.iohk.ethereum.sync.FastSyncItSpec.{BlockchainState, randomAddress}
import io.iohk.ethereum.sync.FastSyncItSpecUtils.FakePeerCustomConfig.defaultConfig
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils.{ByteUtils, Config, NodeStatus, ServerStatus, VmConfig}
import io.iohk.ethereum.vm.EvmConfig
import monix.eval.Task

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.concurrent.duration._
object FastSyncItSpecUtils {
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

  case class HostConfig(
      maxBlocksHeadersPerMessage: Int,
      maxBlocksBodiesPerMessage: Int,
      maxReceiptsPerMessage: Int,
      maxMptComponentsPerMessage: Int
  ) extends FastSyncHostConfiguration

  object HostConfig {
    def apply(): HostConfig = {
      val random: ThreadLocalRandom = ThreadLocalRandom.current()
      new HostConfig(
        maxBlocksHeadersPerMessage = random.nextInt(100, 201),
        maxBlocksBodiesPerMessage = random.nextInt(30, 51),
        maxReceiptsPerMessage = random.nextInt(30, 51),
        maxMptComponentsPerMessage = random.nextInt(100, 201)
      )
    }
  }

  final case class FakePeerCustomConfig(hostConfig: HostConfig)

  object FakePeerCustomConfig {
    val defaultConfig = FakePeerCustomConfig(HostConfig(200, 200, 200, 200))
  }

  class FakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig)
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

    lazy val validators = new MockValidatorsAlwaysSucceed

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

    lazy val fastSync = system.actorOf(
      FastSync.props(
        storagesInstance.storages.fastSyncStateStorage,
        storagesInstance.storages.appStateStorage,
        bl,
        validators,
        peerEventBus,
        etcPeerManager,
        testSyncConfig,
        system.scheduler
      )
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

    def startWithState(): Task[Unit] = {
      Task {
        val currentBest = bl.getBestBlock().header
        val safeTarget = currentBest.number + syncConfig.fastSyncBlockValidationX
        val nextToValidate = currentBest.number + 1
        val syncState = SyncState(currentBest, safeTarget, Seq(), Seq(), 0, 0, currentBest.number, nextToValidate)
        storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)
      }.map(_ => ())
    }

    def startFastSync(): Task[Unit] = Task {
      fastSync ! FastSync.Start
    }

    def waitForFastSyncFinish(): Task[Boolean] = {
      retryUntilWithDelay(Task(storagesInstance.storages.appStateStorage.isFastSyncDone()), 1.second, 90) { isDone =>
        isDone
      }
    }

    // Reads whole trie into memory, if the trie lacks nodes in storage it will be None
    def getBestBlockTrie(): Option[MptNode] = {
      Try {
        val bestBlock = bl.getBestBlock()
        val bestStateRoot = bestBlock.header.stateRoot
        MptTraversals.parseTrieIntoMemory(
          HashNode(bestStateRoot.toArray),
          storagesInstance.storages.stateStorage.getBackingStorage(bestBlock.number)
        )
      }.toOption
    }

    def containsExpectedDataUpToAccountAtBlock(n: BigInt, blockNumber: BigInt): Boolean = {
      @tailrec
      def go(i: BigInt): Boolean = {
        if (i >= n) {
          true
        } else {
          val expectedBalance = i
          val accountAddress = Address(i)
          val accountExpectedCode = ByteString(i.toByteArray)
          val codeHash = kec256(accountExpectedCode)
          val accountExpectedStorageAddresses = (i until i + 20).toList
          val account = bl.getAccount(accountAddress, blockNumber).get
          val code = bl.getEvmCodeByHash(codeHash).get
          val storedData = accountExpectedStorageAddresses.map { addr =>
            ByteUtils.toBigInt(bl.getAccountStorageAt(account.storageRoot, addr, ethCompatibleStorage = true))
          }
          val haveAllStoredData = accountExpectedStorageAddresses.zip(storedData).forall { case (address, value) =>
            address == value
          }

          val dataIsCorrect =
            account.balance.toBigInt == expectedBalance && code == accountExpectedCode && haveAllStoredData
          if (dataIsCorrect) {
            go(i + 1)
          } else {
            false
          }
        }
      }

      go(0)
    }
  }

  object FakePeer {

    def startFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig): Task[FakePeer] = {
      for {
        peer <- Task(new FakePeer(peerName, fakePeerCustomConfig))
        _ <- peer.startPeer()
      } yield peer
    }

    def start1FakePeerRes(
        fakePeerCustomConfig: FakePeerCustomConfig = defaultConfig,
        name: String
    ): Resource[Task, FakePeer] = {
      Resource.make {
        startFakePeer(name, fakePeerCustomConfig)
      } { peer =>
        peer.shutdown()
      }
    }

    def start2FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer)] = {
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
      } yield (peer1, peer2)
    }

    def start3FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig3: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer, FakePeer)] = {
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
        peer3 <- start1FakePeerRes(fakePeerCustomConfig3, "Peer3")
      } yield (peer1, peer2, peer3)
    }
  }

}
