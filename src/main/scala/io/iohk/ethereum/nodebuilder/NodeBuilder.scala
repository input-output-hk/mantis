package io.iohk.ethereum.nodebuilder

import java.time.Clock
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.util.ByteString

import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.Blacklist
import io.iohk.ethereum.blockchain.sync.BlockchainHostActor
import io.iohk.ethereum.blockchain.sync.CacheBasedBlacklist
import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.mining.MiningBuilder
import io.iohk.ethereum.consensus.mining.MiningConfigBuilder
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.BlockchainMetadata
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.NetService.NetServiceConfig
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.controllers.ApisBase
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.ServerActor
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.discovery.DiscoveryConfig
import io.iohk.ethereum.network.discovery.DiscoveryServiceBuilder
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager
import io.iohk.ethereum.network.handshaker.EtcHandshaker
import io.iohk.ethereum.network.handshaker.EtcHandshakerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.security.SSLContextBuilder
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.TransactionHistoryService
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils._

// scalastyle:off number.of.types
trait BlockchainConfigBuilder {
  protected lazy val initBlockchainConfig = Config.blockchains.blockchainConfig
  implicit def blockchainConfig: BlockchainConfig = initBlockchainConfig
}

trait VmConfigBuilder {
  lazy val vmConfig: VmConfig = VmConfig(Config.config)
}

trait SyncConfigBuilder {
  lazy val syncConfig: SyncConfig = SyncConfig(Config.config)
}

trait TxPoolConfigBuilder {
  lazy val txPoolConfig: TxPoolConfig = TxPoolConfig(Config.config)
}

trait FilterConfigBuilder {
  lazy val filterConfig: FilterConfig = FilterConfig(Config.config)
}

trait KeyStoreConfigBuilder {
  lazy val keyStoreConfig: KeyStoreConfig = KeyStoreConfig(Config.config)
}

trait NodeKeyBuilder {
  self: SecureRandomBuilder =>
  lazy val nodeKey: AsymmetricCipherKeyPair = loadAsymmetricCipherKeyPair(Config.nodeKeyFile, secureRandom)
}

trait AsyncConfigBuilder {
  val asyncConfig: AsyncConfig = AsyncConfig(Config.config)
}

trait ActorSystemBuilder {
  implicit lazy val system: ActorSystem = ActorSystem("mantis_system")
}

trait PruningConfigBuilder extends PruningModeComponent {
  lazy val pruningMode: PruningMode = PruningConfig(Config.config).mode
}

trait StorageBuilder {
  lazy val storagesInstance: DataSourceComponent with StoragesComponent with PruningModeComponent =
    Config.Db.dataSource match {
      case "rocksdb" => new RocksDbDataSourceComponent with PruningConfigBuilder with Storages.DefaultStorages
    }
}

trait DiscoveryConfigBuilder extends BlockchainConfigBuilder {
  lazy val discoveryConfig: DiscoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)
}

trait KnownNodesManagerBuilder {
  self: ActorSystemBuilder with StorageBuilder =>

  lazy val knownNodesManagerConfig: KnownNodesManager.KnownNodesManagerConfig =
    KnownNodesManager.KnownNodesManagerConfig(Config.config)

  lazy val knownNodesManager: ActorRef = system.actorOf(
    KnownNodesManager.props(knownNodesManagerConfig, storagesInstance.storages.knownNodesStorage),
    "known-nodes-manager"
  )
}

trait PeerDiscoveryManagerBuilder {
  self: ActorSystemBuilder
    with NodeStatusBuilder
    with DiscoveryConfigBuilder
    with DiscoveryServiceBuilder
    with StorageBuilder =>

  import Scheduler.Implicits.global

  lazy val peerDiscoveryManager: ActorRef = system.actorOf(
    PeerDiscoveryManager.props(
      localNodeId = ByteString(nodeStatusHolder.get.nodeId),
      discoveryConfig,
      storagesInstance.storages.knownNodesStorage,
      discoveryServiceResource(
        discoveryConfig,
        tcpPort = Config.Network.Server.port,
        nodeStatusHolder,
        storagesInstance.storages.knownNodesStorage
      ),
      randomNodeBufferSize = Config.Network.peer.maxOutgoingPeers
    ),
    "peer-discovery-manager"
  )
}

trait BlacklistBuilder {
  private val blacklistSize: Int = 1000 // TODO ETCM-642 move to config
  lazy val blacklist: Blacklist = CacheBasedBlacklist.empty(blacklistSize)
}

trait NodeStatusBuilder {

  self: NodeKeyBuilder =>

  private val nodeStatus =
    NodeStatus(key = nodeKey, serverStatus = ServerStatus.NotListening, discoveryStatus = ServerStatus.NotListening)

  lazy val nodeStatusHolder = new AtomicReference(nodeStatus)
}

trait BlockchainBuilder {
  self: StorageBuilder =>

  private lazy val blockchainMetadata: BlockchainMetadata =
    new BlockchainMetadata(
      storagesInstance.storages.appStateStorage.getBestBlockNumber(),
      storagesInstance.storages.appStateStorage.getLatestCheckpointBlockNumber()
    )
  lazy val blockchainReader: BlockchainReader = BlockchainReader(storagesInstance.storages, blockchainMetadata)
  lazy val blockchainWriter: BlockchainWriter = BlockchainWriter(storagesInstance.storages, blockchainMetadata)
  lazy val blockchain: BlockchainImpl = BlockchainImpl(storagesInstance.storages, blockchainReader, blockchainMetadata)
}

trait BlockQueueBuilder {
  self: BlockchainBuilder with SyncConfigBuilder =>

  lazy val blockQueue: BlockQueue = BlockQueue(blockchain, blockchainReader, syncConfig)
}

trait BlockImportBuilder {
  self: BlockchainBuilder with BlockQueueBuilder with MiningBuilder with ActorSystemBuilder with StorageBuilder =>

  lazy val blockImport: BlockImport = {
    val blockValidation = new BlockValidation(mining, blockchainReader, blockQueue)
    new BlockImport(
      blockchain,
      blockchainReader,
      blockchainWriter,
      blockQueue,
      blockValidation,
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        mining.blockPreparator,
        blockValidation
      ),
      Scheduler(system.dispatchers.lookup("validation-context"))
    )
  }
}

trait ForkResolverBuilder {
  self: BlockchainConfigBuilder =>

  lazy val forkResolverOpt: Option[ForkResolver.EtcForkResolver] =
    blockchainConfig.daoForkConfig.map(new ForkResolver.EtcForkResolver(_))

}

trait HandshakerBuilder {
  self: BlockchainBuilder
    with NodeStatusBuilder
    with StorageBuilder
    with PeerManagerActorBuilder
    with ForkResolverBuilder
    with BlockchainConfigBuilder =>

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = self.forkResolverOpt
      override val nodeStatusHolder: AtomicReference[NodeStatus] = self.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = self.peerConfiguration
      override val blockchain: Blockchain = self.blockchain
      override val blockchainReader: BlockchainReader = self.blockchainReader
      override val appStateStorage: AppStateStorage = self.storagesInstance.storages.appStateStorage
      override val blockchainConfig: BlockchainConfig = self.blockchainConfig
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)
}

trait AuthHandshakerBuilder {
  self: NodeKeyBuilder with SecureRandomBuilder =>

  lazy val authHandshaker: AuthHandshaker = AuthHandshaker(nodeKey, secureRandom)
}

trait PeerEventBusBuilder {
  self: ActorSystemBuilder =>

  lazy val peerEventBus: ActorRef = system.actorOf(PeerEventBusActor.props, "peer-event-bus")
}

trait PeerStatisticsBuilder {
  self: ActorSystemBuilder with PeerEventBusBuilder =>

  // TODO: a candidate to move upwards in trait hierarchy?
  implicit val clock: Clock = Clock.systemUTC()

  lazy val peerStatistics: ActorRef = system.actorOf(
    PeerStatisticsActor.props(
      peerEventBus,
      // `slotCount * slotDuration` should be set so that it's at least as long
      // as any client of the `PeerStatisticsActor` requires.
      slotDuration = Config.Network.peer.statSlotDuration,
      slotCount = Config.Network.peer.statSlotCount
    ),
    "peer-statistics"
  )
}

trait PeerManagerActorBuilder {

  self: ActorSystemBuilder
    with HandshakerBuilder
    with PeerEventBusBuilder
    with AuthHandshakerBuilder
    with PeerDiscoveryManagerBuilder
    with DiscoveryConfigBuilder
    with StorageBuilder
    with KnownNodesManagerBuilder
    with PeerStatisticsBuilder
    with BlacklistBuilder =>

  lazy val peerConfiguration: PeerConfiguration = Config.Network.peer

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

}

trait EtcPeerManagerActorBuilder {
  self: ActorSystemBuilder
    with PeerManagerActorBuilder
    with PeerEventBusBuilder
    with ForkResolverBuilder
    with StorageBuilder =>

  lazy val etcPeerManager: ActorRef = system.actorOf(
    EtcPeerManagerActor.props(peerManager, peerEventBus, storagesInstance.storages.appStateStorage, forkResolverOpt),
    "etc-peer-manager"
  )

}

trait BlockchainHostBuilder {
  self: ActorSystemBuilder
    with BlockchainBuilder
    with StorageBuilder
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder =>

  val blockchainHost: ActorRef = system.actorOf(
    BlockchainHostActor.props(
      blockchainReader,
      storagesInstance.storages.evmCodeStorage,
      peerConfiguration,
      peerEventBus,
      etcPeerManager
    ),
    "blockchain-host"
  )

}

trait ServerActorBuilder {

  self: ActorSystemBuilder with NodeStatusBuilder with BlockchainBuilder with PeerManagerActorBuilder =>

  lazy val networkConfig: Config.Network.type = Config.Network

  lazy val server: ActorRef = system.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

}

trait Web3ServiceBuilder {
  lazy val web3Service = new Web3Service
}

trait NetServiceBuilder {
  this: PeerManagerActorBuilder with NodeStatusBuilder =>

  lazy val netServiceConfig: NetServiceConfig = NetServiceConfig(Config.config)

  lazy val netService = new NetService(nodeStatusHolder, peerManager, netServiceConfig)
}

trait PendingTransactionsManagerBuilder {
  def pendingTransactionsManager: ActorRef
}
object PendingTransactionsManagerBuilder {
  trait Default extends PendingTransactionsManagerBuilder {
    self: ActorSystemBuilder
      with PeerManagerActorBuilder
      with EtcPeerManagerActorBuilder
      with PeerEventBusBuilder
      with TxPoolConfigBuilder =>

    lazy val pendingTransactionsManager: ActorRef =
      system.actorOf(PendingTransactionsManager.props(txPoolConfig, peerManager, etcPeerManager, peerEventBus))
  }
}

trait TransactionHistoryServiceBuilder {
  def transactionHistoryService: TransactionHistoryService
}
object TransactionHistoryServiceBuilder {
  trait Default extends TransactionHistoryServiceBuilder {
    self: BlockchainBuilder with PendingTransactionsManagerBuilder with TxPoolConfigBuilder =>
    lazy val transactionHistoryService =
      new TransactionHistoryService(
        blockchain,
        blockchainReader,
        pendingTransactionsManager,
        txPoolConfig.getTransactionFromPoolTimeout
      )
  }
}

trait FilterManagerBuilder {
  self: ActorSystemBuilder
    with BlockchainBuilder
    with StorageBuilder
    with KeyStoreBuilder
    with PendingTransactionsManagerBuilder
    with FilterConfigBuilder
    with TxPoolConfigBuilder
    with MiningBuilder =>

  lazy val filterManager: ActorRef =
    system.actorOf(
      FilterManager.props(
        blockchain,
        blockchainReader,
        mining.blockGenerator,
        keyStore,
        pendingTransactionsManager,
        filterConfig,
        txPoolConfig
      ),
      "filter-manager"
    )
}

trait DebugServiceBuilder {
  self: EtcPeerManagerActorBuilder with PeerManagerActorBuilder =>

  lazy val debugService = new DebugService(peerManager, etcPeerManager)
}

trait EthProofServiceBuilder {
  self: StorageBuilder with BlockchainBuilder with BlockchainConfigBuilder with MiningBuilder =>

  lazy val ethProofService: ProofService = new EthProofService(
    blockchain,
    blockchainReader,
    mining.blockGenerator,
    blockchainConfig.ethCompatibleStorage
  )
}

trait EthInfoServiceBuilder {
  self: StorageBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with MiningBuilder
    with StxLedgerBuilder
    with KeyStoreBuilder
    with SyncControllerBuilder
    with AsyncConfigBuilder =>

  lazy val ethInfoService = new EthInfoService(
    blockchain,
    blockchainReader,
    blockchainConfig,
    mining,
    stxLedger,
    keyStore,
    syncController,
    Capability.best(blockchainConfig.capabilities),
    asyncConfig.askTimeout
  )
}

trait EthMiningServiceBuilder {
  self: BlockchainBuilder
    with BlockchainConfigBuilder
    with MiningBuilder
    with JSONRpcConfigBuilder
    with OmmersPoolBuilder
    with SyncControllerBuilder
    with PendingTransactionsManagerBuilder
    with TxPoolConfigBuilder =>

  lazy val ethMiningService = new EthMiningService(
    blockchainReader,
    mining,
    jsonRpcConfig,
    ommersPool,
    syncController,
    pendingTransactionsManager,
    txPoolConfig.getTransactionFromPoolTimeout,
    this
  )
}
trait EthTxServiceBuilder {
  self: BlockchainBuilder
    with PendingTransactionsManagerBuilder
    with MiningBuilder
    with TxPoolConfigBuilder
    with StorageBuilder =>

  lazy val ethTxService = new EthTxService(
    blockchain,
    blockchainReader,
    mining,
    pendingTransactionsManager,
    txPoolConfig.getTransactionFromPoolTimeout,
    storagesInstance.storages.transactionMappingStorage
  )
}

trait EthBlocksServiceBuilder {
  self: BlockchainBuilder with MiningBuilder with BlockQueueBuilder =>

  lazy val ethBlocksService = new EthBlocksService(blockchain, blockchainReader, mining, blockQueue)
}

trait EthUserServiceBuilder {
  self: BlockchainBuilder with BlockchainConfigBuilder with MiningBuilder with StorageBuilder =>

  lazy val ethUserService = new EthUserService(
    blockchain,
    blockchainReader,
    mining,
    storagesInstance.storages.evmCodeStorage,
    this
  )
}

trait EthFilterServiceBuilder {
  self: FilterManagerBuilder with FilterConfigBuilder =>

  lazy val ethFilterService = new EthFilterService(
    filterManager,
    filterConfig
  )
}

trait PersonalServiceBuilder {
  self: KeyStoreBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with PendingTransactionsManagerBuilder
    with StorageBuilder
    with TxPoolConfigBuilder =>

  lazy val personalService = new PersonalService(
    keyStore,
    blockchain,
    blockchainReader,
    pendingTransactionsManager,
    txPoolConfig,
    this
  )
}

trait QaServiceBuilder {
  self: MiningBuilder
    with SyncControllerBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with CheckpointBlockGeneratorBuilder =>

  lazy val qaService =
    new QAService(
      mining,
      blockchainReader,
      checkpointBlockGenerator,
      blockchainConfig,
      syncController
    )
}

trait CheckpointingServiceBuilder {
  self: BlockchainBuilder
    with SyncControllerBuilder
    with StxLedgerBuilder
    with CheckpointBlockGeneratorBuilder
    with BlockQueueBuilder =>

  lazy val checkpointingService =
    new CheckpointingService(
      blockchain,
      blockchainReader,
      blockQueue,
      checkpointBlockGenerator,
      syncController
    )
}

trait MantisServiceBuilder {
  self: TransactionHistoryServiceBuilder with JSONRpcConfigBuilder =>

  lazy val mantisService = new MantisService(transactionHistoryService, jsonRpcConfig)
}

trait KeyStoreBuilder {
  self: SecureRandomBuilder with KeyStoreConfigBuilder =>
  lazy val keyStore: KeyStore = new KeyStoreImpl(keyStoreConfig, secureRandom)
}

trait ApisBuilder extends ApisBase {
  object Apis {
    val Eth = "eth"
    val Web3 = "web3"
    val Net = "net"
    val Personal = "personal"
    val Mantis = "mantis"
    val Debug = "debug"
    val Rpc = "rpc"
    val Test = "test"
    val Iele = "iele"
    val Qa = "qa"
    val Checkpointing = "checkpointing"
  }

  import Apis._
  override def available: List[String] = List(Eth, Web3, Net, Personal, Mantis, Debug, Test, Iele, Qa, Checkpointing)
}

trait JSONRpcConfigBuilder {
  self: ApisBuilder =>

  lazy val availableApis: List[String] = available
  lazy val jsonRpcConfig: JsonRpcConfig = JsonRpcConfig(Config.config, availableApis)
}

trait JSONRpcControllerBuilder {
  this: Web3ServiceBuilder
    with EthInfoServiceBuilder
    with EthProofServiceBuilder
    with EthMiningServiceBuilder
    with EthBlocksServiceBuilder
    with EthTxServiceBuilder
    with EthUserServiceBuilder
    with EthFilterServiceBuilder
    with NetServiceBuilder
    with PersonalServiceBuilder
    with DebugServiceBuilder
    with JSONRpcConfigBuilder
    with QaServiceBuilder
    with CheckpointingServiceBuilder
    with MantisServiceBuilder =>

  protected def testService: Option[TestService] = None

  lazy val jsonRpcController =
    new JsonRpcController(
      web3Service,
      netService,
      ethInfoService,
      ethMiningService,
      ethBlocksService,
      ethTxService,
      ethUserService,
      ethFilterService,
      personalService,
      testService,
      debugService,
      qaService,
      checkpointingService,
      mantisService,
      ethProofService,
      jsonRpcConfig
    )
}

trait JSONRpcHealthcheckerBuilder {
  this: NetServiceBuilder
    with EthBlocksServiceBuilder
    with JSONRpcConfigBuilder
    with AsyncConfigBuilder
    with SyncControllerBuilder =>
  lazy val jsonRpcHealthChecker: JsonRpcHealthChecker =
    new NodeJsonRpcHealthChecker(
      netService,
      ethBlocksService,
      syncController,
      jsonRpcConfig.healthConfig,
      asyncConfig
    )
}

trait JSONRpcHttpServerBuilder {
  self: ActorSystemBuilder
    with BlockchainBuilder
    with JSONRpcControllerBuilder
    with JSONRpcHealthcheckerBuilder
    with SecureRandomBuilder
    with JSONRpcConfigBuilder
    with SSLContextBuilder =>

  lazy val maybeJsonRpcHttpServer: Either[String, JsonRpcHttpServer] =
    JsonRpcHttpServer(
      jsonRpcController,
      jsonRpcHealthChecker,
      jsonRpcConfig.httpServerConfig,
      secureRandom,
      () => sslContext("mantis.network.rpc.http")
    )
}

trait JSONRpcIpcServerBuilder {
  self: ActorSystemBuilder with JSONRpcControllerBuilder with JSONRpcConfigBuilder =>

  lazy val jsonRpcIpcServer = new JsonRpcIpcServer(jsonRpcController, jsonRpcConfig.ipcServerConfig)
}

trait OmmersPoolBuilder {
  self: ActorSystemBuilder with BlockchainBuilder with MiningConfigBuilder =>

  lazy val ommersPoolSize: Int = 30 // FIXME For this we need EthashConfig, which means Ethash consensus
  lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(blockchainReader, ommersPoolSize))
}

trait VmBuilder {
  self: ActorSystemBuilder with BlockchainConfigBuilder with VmConfigBuilder =>

  lazy val vm: VMImpl = VmSetup.vm(vmConfig, blockchainConfig, testMode = false)
}

trait StxLedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with StorageBuilder
    with SyncConfigBuilder
    with MiningBuilder
    with ActorSystemBuilder =>

  lazy val stxLedger: StxLedger =
    new StxLedger(
      blockchain,
      blockchainReader,
      storagesInstance.storages.evmCodeStorage,
      mining.blockPreparator,
      this
    )
}

trait CheckpointBlockGeneratorBuilder {
  lazy val checkpointBlockGenerator = new CheckpointBlockGenerator()
}

trait SyncControllerBuilder {

  self: ActorSystemBuilder
    with ServerActorBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with BlockImportBuilder
    with NodeStatusBuilder
    with StorageBuilder
    with StxLedgerBuilder
    with PeerEventBusBuilder
    with PendingTransactionsManagerBuilder
    with OmmersPoolBuilder
    with EtcPeerManagerActorBuilder
    with SyncConfigBuilder
    with ShutdownHookBuilder
    with MiningBuilder
    with BlacklistBuilder =>

  lazy val syncController: ActorRef = system.actorOf(
    SyncController.props(
      storagesInstance.storages.appStateStorage,
      blockchain,
      blockchainReader,
      blockchainWriter,
      storagesInstance.storages.evmCodeStorage,
      storagesInstance.storages.stateStorage,
      storagesInstance.storages.nodeStorage,
      storagesInstance.storages.fastSyncStateStorage,
      blockImport,
      mining.validators,
      peerEventBus,
      pendingTransactionsManager,
      ommersPool,
      etcPeerManager,
      blacklist,
      syncConfig,
      this
    ),
    "sync-controller"
  )

}

trait PortForwardingBuilder {
  self: DiscoveryConfigBuilder =>

  import Scheduler.Implicits.global

  private val portForwarding = PortForwarder
    .openPorts(
      Seq(Config.Network.Server.port),
      Seq(discoveryConfig.port).filter(_ => discoveryConfig.discoveryEnabled)
    )
    .whenA(Config.Network.automaticPortForwarding)
    .allocated
    .map(_._2)

  // reference to a task that produces the release task,
  // memoized to prevent running multiple port forwarders at once
  private val portForwardingRelease = new AtomicReference(Option.empty[Task[Task[Unit]]])

  def startPortForwarding(): Future[Unit] = {
    portForwardingRelease.compareAndSet(None, Some(portForwarding.memoize))
    portForwardingRelease.get().fold(Future.unit)(_.runToFuture.void)
  }

  def stopPortForwarding(): Future[Unit] =
    portForwardingRelease.getAndSet(None).fold(Future.unit)(_.flatten.runToFuture)
}

trait ShutdownHookBuilder {
  self: Logger =>
  def shutdown(): Unit = {
    /* No default behaviour during shutdown. */
  }

  lazy val shutdownTimeoutDuration: Duration = Config.shutdownTimeout

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit =
      shutdown()
  })

  def shutdownOnError[A](f: => A): A =
    Try(f) match {
      case Success(v) => v
      case Failure(t) =>
        log.error(t.getMessage, t)
        shutdown()
        throw t
    }
}

object ShutdownHookBuilder extends ShutdownHookBuilder with Logger

trait GenesisDataLoaderBuilder {
  self: BlockchainBuilder with StorageBuilder =>

  lazy val genesisDataLoader =
    new GenesisDataLoader(
      blockchainReader,
      blockchainWriter,
      storagesInstance.storages.stateStorage
    )

}

/** Provides the basic functionality of a Node, except the mining algorithm.
  * The latter is loaded dynamically based on configuration.
  *
  * @see [[MiningBuilder MiningBuilder]],
  *      [[MiningConfigBuilder ConsensusConfigBuilder]]
  */
trait Node
    extends SecureRandomBuilder
    with NodeKeyBuilder
    with ActorSystemBuilder
    with StorageBuilder
    with BlockchainBuilder
    with BlockQueueBuilder
    with BlockImportBuilder
    with NodeStatusBuilder
    with ForkResolverBuilder
    with HandshakerBuilder
    with PeerStatisticsBuilder
    with PeerManagerActorBuilder
    with ServerActorBuilder
    with SyncControllerBuilder
    with Web3ServiceBuilder
    with EthInfoServiceBuilder
    with EthProofServiceBuilder
    with EthMiningServiceBuilder
    with EthBlocksServiceBuilder
    with EthTxServiceBuilder
    with EthUserServiceBuilder
    with EthFilterServiceBuilder
    with NetServiceBuilder
    with PersonalServiceBuilder
    with DebugServiceBuilder
    with QaServiceBuilder
    with CheckpointingServiceBuilder
    with MantisServiceBuilder
    with KeyStoreBuilder
    with ApisBuilder
    with JSONRpcConfigBuilder
    with JSONRpcHealthcheckerBuilder
    with JSONRpcControllerBuilder
    with SSLContextBuilder
    with JSONRpcHttpServerBuilder
    with JSONRpcIpcServerBuilder
    with ShutdownHookBuilder
    with Logger
    with GenesisDataLoaderBuilder
    with BlockchainConfigBuilder
    with VmConfigBuilder
    with PeerEventBusBuilder
    with PendingTransactionsManagerBuilder.Default
    with OmmersPoolBuilder
    with EtcPeerManagerActorBuilder
    with BlockchainHostBuilder
    with FilterManagerBuilder
    with FilterConfigBuilder
    with TxPoolConfigBuilder
    with AuthHandshakerBuilder
    with PruningConfigBuilder
    with PeerDiscoveryManagerBuilder
    with DiscoveryServiceBuilder
    with DiscoveryConfigBuilder
    with KnownNodesManagerBuilder
    with SyncConfigBuilder
    with VmBuilder
    with MiningBuilder
    with MiningConfigBuilder
    with StxLedgerBuilder
    with KeyStoreConfigBuilder
    with AsyncConfigBuilder
    with CheckpointBlockGeneratorBuilder
    with TransactionHistoryServiceBuilder.Default
    with PortForwardingBuilder
    with BlacklistBuilder
