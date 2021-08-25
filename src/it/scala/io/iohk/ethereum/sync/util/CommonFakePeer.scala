package io.iohk.ethereum.sync.util

import java.net.InetSocketAddress
import java.nio.file.Files
import java.nio.file.Path
import java.time.Clock
import java.util.concurrent.atomic.AtomicReference
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import akka.util.Timeout
import monix.eval.Task

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.blockchain.sync.BlockchainHostActor
import io.iohk.ethereum.blockchain.sync.CacheBasedBlacklist
import io.iohk.ethereum.blockchain.sync.TestSyncConfig
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.db.components.RocksDbDataSourceComponent
import io.iohk.ethereum.db.components.Storages
import io.iohk.ethereum.db.dataSource.RocksDbConfig
import io.iohk.ethereum.db.dataSource.RocksDbDataSource
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.Namespaces
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.{
  Block,
  BlockMetadataProxy,
  Blockchain,
  BlockchainImpl,
  BlockchainReader,
  BlockchainWriter,
  ChainWeight
}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.KnownNodesManager
import io.iohk.ethereum.network.PeerEventBusActor
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.PeerManagerActor.FastSyncHostConfiguration
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.PeerStatisticsActor
import io.iohk.ethereum.network.ServerActor
import io.iohk.ethereum.network.discovery.DiscoveryConfig
import io.iohk.ethereum.network.discovery.Node
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.DiscoveredNodesInfo
import io.iohk.ethereum.network.handshaker.EtcHandshaker
import io.iohk.ethereum.network.handshaker.EtcHandshakerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.sync.util.SyncCommonItSpec._
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm.EvmConfig

abstract class CommonFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig)
    extends SecureRandomBuilder
    with TestSyncConfig
    with BlockchainConfigBuilder {
  implicit val akkaTimeout: Timeout = Timeout(5.second)

  val config = Config.config

  import scala.language.postfixOps

  implicit val clock: Clock = Clock.systemUTC()

  implicit val system: ActorSystem = ActorSystem(peerName)

  val peerDiscoveryManager: ActorRef = TestProbe().ref

  val nodeKey: AsymmetricCipherKeyPair = io.iohk.ethereum.crypto.generateKeyPair(secureRandom)

  private val nodeStatus =
    NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )

  lazy val tempDir: Path = Files.createTempDirectory("temp-fast-sync")

  def getRockDbTestConfig(dbPath: String): RocksDbConfig =
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

  sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  lazy val nodeStatusHolder = new AtomicReference(nodeStatus)
  lazy val storagesInstance: RocksDbDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages =
    new RocksDbDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages {
      override lazy val dataSource: RocksDbDataSource =
        RocksDbDataSource(getRockDbTestConfig(tempDir.toAbsolutePath.toString), Namespaces.nsSeq)
    }
  implicit override lazy val blockchainConfig: BlockchainConfig = Config.blockchains.blockchainConfig
  lazy val discoveryConfig: DiscoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)

  /** Default persist interval is 20s, which is too long for tests. As in all tests we treat peer as connected when
    * it is persisted in storage.
    */
  lazy val knownNodesManagerConfig: KnownNodesManager.KnownNodesManagerConfig =
    KnownNodesManager.KnownNodesManagerConfig(config).copy(persistInterval = 1.seconds)

  lazy val knownNodesManager: ActorRef = system.actorOf(
    KnownNodesManager.props(
      knownNodesManagerConfig,
      storagesInstance.storages.knownNodesStorage
    )
  )

  val blockMetadataProxy: BlockMetadataProxy = new BlockMetadataProxy(storagesInstance.storages.blockMetadataStorage)
  val blockchainReader: BlockchainReader = BlockchainReader(storagesInstance.storages)
  val blockchainWriter: BlockchainWriter = BlockchainWriter(storagesInstance.storages)
  val bl: BlockchainImpl = BlockchainImpl(storagesInstance.storages, blockchainReader)
  val evmCodeStorage = storagesInstance.storages.evmCodeStorage

  val genesis: Block = Block(
    Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
    Fixtures.Blocks.Genesis.body
  )
  val genesisWeight: ChainWeight = ChainWeight.zero.increase(genesis.header)

  blockchainWriter.save(genesis, Seq(), genesisWeight, saveAsBestBlock = true)

  lazy val nh = nodeStatusHolder

  val peerConf: PeerConfiguration = new PeerConfiguration {
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
    override val minOutgoingPeers = 5
    override val maxOutgoingPeers = 10
    override val maxIncomingPeers = 5
    override val maxPendingPeers = 5
    override val pruneIncomingPeers = 0
    override val minPruneAge = 1.minute
    override val networkId: Int = 1

    override val updateNodesInitialDelay: FiniteDuration = 5.seconds
    override val updateNodesInterval: FiniteDuration = 20.seconds
    override val shortBlacklistDuration: FiniteDuration = 1.minute
    override val longBlacklistDuration: FiniteDuration = 3.minutes
    override val statSlotDuration: FiniteDuration = 1.minute
    override val statSlotCount: Int = 30
  }

  lazy val peerEventBus: ActorRef = system.actorOf(PeerEventBusActor.props, "peer-event-bus")

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = None
      override val nodeStatusHolder: AtomicReference[NodeStatus] = nh
      override val peerConfiguration: PeerConfiguration = peerConf
      override val blockchain: Blockchain = CommonFakePeer.this.bl
      override val blockchainReader: BlockchainReader = CommonFakePeer.this.blockchainReader
      override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
      override val blockchainConfig: BlockchainConfig = Config.blockchains.blockchainConfig
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)

  lazy val authHandshaker: AuthHandshaker = AuthHandshaker(nodeKey, secureRandom)

  lazy val peerStatistics: ActorRef =
    system.actorOf(PeerStatisticsActor.props(peerEventBus, slotDuration = 1.minute, slotCount = 30))

  lazy val blacklist: CacheBasedBlacklist = CacheBasedBlacklist.empty(1000)

  lazy val peerManager: ActorRef = system.actorOf(
    PeerManagerActor.props(
      peerDiscoveryManager,
      Config.Network.peer,
      peerEventBus,
      knownNodesManager,
      peerStatistics,
      handshaker,
      authHandshaker,
      discoveryConfig,
      blacklist,
      blockchainConfig.capabilities
    ),
    "peer-manager"
  )

  lazy val etcPeerManager: ActorRef = system.actorOf(
    EtcPeerManagerActor.props(peerManager, peerEventBus, storagesInstance.storages.appStateStorage, None),
    "etc-peer-manager"
  )

  val blockchainHost: ActorRef =
    system.actorOf(
      BlockchainHostActor
        .props(blockchainReader, storagesInstance.storages.evmCodeStorage, peerConf, peerEventBus, etcPeerManager),
      "blockchain-host"
    )

  lazy val server: ActorRef = system.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

  val listenAddress: InetSocketAddress = randomAddress()

  lazy val node: Node =
    Node(ByteString(nodeStatus.nodeId), listenAddress.getAddress, listenAddress.getPort, listenAddress.getPort)

  lazy val vmConfig: VmConfig = VmConfig(Config.config)

  val testSyncConfig: Config.SyncConfig = syncConfig.copy(
    minPeersToChoosePivotBlock = 1,
    peersScanInterval = 5.milliseconds,
    blockHeadersPerRequest = 200,
    blockBodiesPerRequest = 50,
    receiptsPerRequest = 50,
    fastSyncThrottle = 10.milliseconds,
    startRetryInterval = 50.milliseconds,
    nodesPerRequest = 200,
    maxTargetDifference = 1,
    syncRetryInterval = 50.milliseconds,
    blacklistDuration = 100.seconds,
    fastSyncMaxBatchRetries = 2,
    fastSyncBlockValidationN = 200
  )

  lazy val broadcaster = new BlockBroadcast(etcPeerManager)

  lazy val broadcasterActor: ActorRef = system.actorOf(
    BlockBroadcasterActor.props(broadcaster, peerEventBus, etcPeerManager, blacklist, testSyncConfig, system.scheduler)
  )

  private def getMptForBlock(block: Block) =
    InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      bl.getBackingMptStorage(block.number),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      blockchainConfig.accountStartNonce,
      block.header.stateRoot,
      noEmptyAccounts = EvmConfig.forBlock(block.number, blockchainConfig).noEmptyAccounts,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
    )

  private def broadcastBlock(block: Block, weight: ChainWeight) =
    broadcasterActor ! BroadcastBlock(BlockToBroadcast(block, weight))

  def getCurrentState(): BlockchainState = {
    val bestBlock = blockchainReader.getBestBlock().get
    val currentWorldState = getMptForBlock(bestBlock)
    val currentWeight = blockchainReader.getChainWeightByHash(bestBlock.hash).get
    BlockchainState(bestBlock, currentWorldState, currentWeight)
  }

  def startPeer(): Task[Unit] =
    for {
      _ <- Task {
        peerManager ! PeerManagerActor.StartConnecting
        server ! ServerActor.StartServer(listenAddress)
      }
      _ <- retryUntilWithDelay(Task(nodeStatusHolder.get()), 1.second, 5) { status =>
        status.serverStatus == Listening(listenAddress)
      }
    } yield ()

  def shutdown(): Task[Unit] =
    for {
      _ <- Task.deferFuture(system.terminate())
      _ <- Task(storagesInstance.dataSource.destroy())
    } yield ()

  def connectToPeers(nodes: Set[Node]): Task[Unit] =
    for {
      _ <- Task {
        peerManager ! DiscoveredNodesInfo(nodes)
      }
      _ <- retryUntilWithDelay(Task(storagesInstance.storages.knownNodesStorage.getKnownNodes()), 1.second, 5) {
        knownNodes =>
          val requestedNodes = nodes.map(_.id)
          val currentNodes = knownNodes.map(Node.fromUri).map(_.id)
          requestedNodes.subsetOf(currentNodes)
      }
    } yield ()

  private def createChildBlock(parent: Block, parentWeight: ChainWeight, parentWorld: InMemoryWorldStateProxy)(
      updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
  ): (Block, ChainWeight, InMemoryWorldStateProxy) = {
    val newBlockNumber = parent.header.number + 1
    val newWorld = updateWorldForBlock(newBlockNumber, parentWorld)
    val newBlock = parent.copy(header =
      parent.header.copy(parentHash = parent.header.hash, number = newBlockNumber, stateRoot = newWorld.stateRootHash)
    )
    val newWeight = parentWeight.increase(newBlock.header)
    (newBlock, newWeight, parentWorld)
  }

  private def generateInvalidBlock(
      currentBestBlock: Block
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
    Task {
      val currentWorld = getMptForBlock(currentBestBlock)

      val newBlockNumber = currentBestBlock.header.number + 1
      val newWorld = updateWorldForBlock(newBlockNumber, currentWorld)

      // The child block is made invalid by not properly updating its parent hash.
      val childBlock =
        currentBestBlock.copy(header =
          currentBestBlock.header.copy(
            number = newBlockNumber,
            stateRoot = newWorld.stateRootHash
          )
        )
      val newWeight = ChainWeight.totalDifficultyOnly(1)

      broadcastBlock(childBlock, newWeight)
      blockchainWriter.save(childBlock, Seq(), newWeight, saveAsBestBlock = true)
    }

  private def generateValidBlock(
      currentBestBlock: Block
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
    Task {
      val currentWeight = blockchainReader.getChainWeightByHash(currentBestBlock.hash).get
      val currentWorld = getMptForBlock(currentBestBlock)
      val (newBlock, newWeight, _) =
        createChildBlock(currentBestBlock, currentWeight, currentWorld)(updateWorldForBlock)
      blockchainWriter.save(newBlock, Seq(), newWeight, saveAsBestBlock = true)
      broadcastBlock(newBlock, newWeight)
    }

  def importBlocksUntil(
      n: BigInt
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
    Task(blockchainReader.getBestBlock()).flatMap { block =>
      if (block.get.number >= n) {
        Task(())
      } else {
        generateValidBlock(block.get)(updateWorldForBlock).flatMap(_ => importBlocksUntil(n)(updateWorldForBlock))
      }
    }

  def importInvalidBlocks(
      from: BigInt,
      to: BigInt
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
    Task(blockchainReader.getBestBlock()).flatMap { block =>
      if (block.get.number >= to) {
        Task(())
      } else if (block.get.number >= from) {
        generateInvalidBlock(block.get)(updateWorldForBlock).flatMap(_ =>
          importInvalidBlocks(from, to)(updateWorldForBlock)
        )
      } else {
        generateValidBlock(block.get)(updateWorldForBlock).flatMap(_ =>
          importInvalidBlocks(from, to)(updateWorldForBlock)
        )
      }

    }

  def importInvalidBlockNumbers(
      from: BigInt,
      to: BigInt
  )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
    Task(blockchainReader.getBestBlock()).flatMap { block =>
      if (block.get.number >= to) {
        Task(())
      } else if (block.get.number >= from) {
        generateInvalidBlock(block.get)(updateWorldForBlock).flatMap(_ =>
          importInvalidBlockNumbers(from, to)(updateWorldForBlock)
        )
      } else {
        importBlocksUntil(from)(updateWorldForBlock)
      }

    }

}
