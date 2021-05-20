package io.iohk.ethereum.nodebuilder

import java.time.Clock
import akka.actor.{ActorRef, ActorSystem}
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.{Blacklist, BlockchainHostActor, CacheBasedBlacklist, SyncController}
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.NetService.NetServiceConfig
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.security.{SSLContextBuilder, SecureRandomBuilder}
import io.iohk.ethereum.jsonrpc.server.controllers.ApisBase
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer
import io.iohk.ethereum.keystore.{KeyStore, KeyStoreImpl}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, DiscoveryServiceBuilder, PeerDiscoveryManager}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor, _}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.testmode.{TestEthBlockServiceWrapper, TestLedgerBuilder, TestmodeConsensusBuilder}
import io.iohk.ethereum.transactions.{PendingTransactionsManager, TransactionHistoryService}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils._

import java.util.concurrent.atomic.AtomicReference
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import akka.util.ByteString
import monix.execution.Scheduler
import cats.implicits._
import monix.eval.Task

// scalastyle:off number.of.types
trait BlockchainConfigBuilder {
  lazy val blockchainConfig = Config.blockchains.blockchainConfig
}

trait VmConfigBuilder {
  lazy val vmConfig = VmConfig(Config.config)
}

trait SyncConfigBuilder {
  lazy val syncConfig = SyncConfig(Config.config)
}

trait TxPoolConfigBuilder {
  lazy val txPoolConfig = TxPoolConfig(Config.config)
}

trait FilterConfigBuilder {
  lazy val filterConfig = FilterConfig(Config.config)
}

trait KeyStoreConfigBuilder {
  lazy val keyStoreConfig = KeyStoreConfig(Config.config)
}

trait NodeKeyBuilder {
  self: SecureRandomBuilder =>
  lazy val nodeKey: AsymmetricCipherKeyPair = loadAsymmetricCipherKeyPair(Config.nodeKeyFile, secureRandom)
}

trait AsyncConfigBuilder {
  val asyncConfig = AsyncConfig(Config.config)
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
  lazy val discoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)
}

trait KnownNodesManagerBuilder {
  self: ActorSystemBuilder with StorageBuilder =>

  lazy val knownNodesManagerConfig = KnownNodesManager.KnownNodesManagerConfig(Config.config)

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

  lazy val blockchain: BlockchainImpl = BlockchainImpl(storagesInstance.storages)
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
    with ForkResolverBuilder =>

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = self.forkResolverOpt
      override val nodeStatusHolder: AtomicReference[NodeStatus] = self.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = self.peerConfiguration
      override val blockchain: Blockchain = self.blockchain
      override val appStateStorage: AppStateStorage = self.storagesInstance.storages.appStateStorage
      override val protocolVersion: Int = Config.Network.protocolVersion
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
  implicit val clock = Clock.systemUTC()

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
      EthereumMessageDecoder,
      discoveryConfig,
      blacklist,
      Config.Network.protocolVersion
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
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder =>

  val blockchainHost: ActorRef = system.actorOf(
    BlockchainHostActor.props(blockchain, peerConfiguration, peerEventBus, etcPeerManager),
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

  lazy val netServiceConfig = NetServiceConfig(Config.config)

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
      new TransactionHistoryService(blockchain, pendingTransactionsManager, txPoolConfig.getTransactionFromPoolTimeout)
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
    with ConsensusBuilder =>

  lazy val filterManager: ActorRef =
    system.actorOf(
      FilterManager.props(
        blockchain,
        consensus.blockGenerator,
        storagesInstance.storages.appStateStorage,
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

trait TestServiceBuilder {
  self: BlockchainBuilder
    with PendingTransactionsManagerBuilder
    with ConsensusConfigBuilder
    with BlockchainConfigBuilder
    with VmBuilder
    with TestmodeConsensusBuilder
    with TestLedgerBuilder =>

  lazy val testService =
    new TestService(blockchain, pendingTransactionsManager, consensusConfig, consensus, testLedgerWrapper)(scheduler)
}

trait TestEthBlockServiceBuilder extends EthBlocksServiceBuilder {
  self: BlockchainBuilder with TestLedgerBuilder with ConsensusBuilder =>
  override lazy val ethBlocksService = new TestEthBlockServiceWrapper(blockchain, ledger, consensus)
}

trait EthProofServiceBuilder {
  self: StorageBuilder with BlockchainBuilder with BlockchainConfigBuilder with ConsensusBuilder =>

  lazy val ethProofService: ProofService = new EthProofService(
    blockchain,
    consensus.blockGenerator,
    blockchainConfig.ethCompatibleStorage
  )
}

trait EthInfoServiceBuilder {
  self: StorageBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with LedgerBuilder
    with KeyStoreBuilder
    with SyncControllerBuilder
    with AsyncConfigBuilder =>

  lazy val ethInfoService = new EthInfoService(
    blockchain,
    blockchainConfig,
    ledger,
    stxLedger,
    keyStore,
    syncController,
    Config.Network.protocolVersion,
    asyncConfig.askTimeout
  )
}

trait EthMiningServiceBuilder {
  self: BlockchainBuilder
    with BlockchainConfigBuilder
    with LedgerBuilder
    with JSONRpcConfigBuilder
    with OmmersPoolBuilder
    with SyncControllerBuilder
    with PendingTransactionsManagerBuilder
    with TxPoolConfigBuilder =>

  lazy val ethMiningService = new EthMiningService(
    blockchain,
    blockchainConfig,
    ledger,
    jsonRpcConfig,
    ommersPool,
    syncController,
    pendingTransactionsManager,
    txPoolConfig.getTransactionFromPoolTimeout
  )
}
trait EthTxServiceBuilder {
  self: BlockchainBuilder with PendingTransactionsManagerBuilder with LedgerBuilder with TxPoolConfigBuilder =>

  lazy val ethTxService = new EthTxService(
    blockchain,
    ledger,
    pendingTransactionsManager,
    txPoolConfig.getTransactionFromPoolTimeout
  )
}

trait EthBlocksServiceBuilder {
  self: BlockchainBuilder with LedgerBuilder =>

  lazy val ethBlocksService = new EthBlocksService(blockchain, ledger)
}

trait EthUserServiceBuilder {
  self: BlockchainBuilder with BlockchainConfigBuilder with LedgerBuilder =>

  lazy val ethUserService = new EthUserService(
    blockchain,
    ledger,
    blockchainConfig
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
    pendingTransactionsManager,
    storagesInstance.storages.appStateStorage,
    blockchainConfig,
    txPoolConfig
  )
}

trait QaServiceBuilder {
  self: ConsensusBuilder
    with SyncControllerBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with CheckpointBlockGeneratorBuilder =>

  lazy val qaService =
    new QAService(
      consensus,
      blockchain,
      checkpointBlockGenerator,
      blockchainConfig,
      syncController
    )
}

trait CheckpointingServiceBuilder {
  self: BlockchainBuilder with SyncControllerBuilder with LedgerBuilder with CheckpointBlockGeneratorBuilder =>

  lazy val checkpointingService =
    new CheckpointingService(
      blockchain,
      ledger,
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

  private lazy val testService =
    if (Config.testmode) Some(this.asInstanceOf[TestServiceBuilder].testService)
    else None

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

  lazy val maybeJsonRpcHttpServer =
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
  self: ActorSystemBuilder with BlockchainBuilder with ConsensusConfigBuilder =>

  lazy val ommersPoolSize: Int = 30 // FIXME For this we need EthashConfig, which means Ethash consensus
  lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(blockchain, ommersPoolSize))
}

trait VmBuilder {
  self: ActorSystemBuilder with BlockchainConfigBuilder with VmConfigBuilder =>

  lazy val vm: VMImpl = VmSetup.vm(vmConfig, blockchainConfig, testMode = false)
}

trait LedgerBuilder {
  def ledger: Ledger
  def stxLedger: StxLedger
}

trait StdLedgerBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder
    with ActorSystemBuilder =>

  val scheduler: Scheduler = Scheduler(system.dispatchers.lookup("validation-context"))

  /** This is used in tests, which need the more specific type
    *
    * @note See if the APIs that the tests need can be promoted to the Ledger interface.
    * @note In fact, most if all these APIs are now being delegated to the BlockPreparator,
    *       so a refactoring should probably take that into account.
    */
  protected def newLedger(): LedgerImpl =
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus, scheduler)

  override lazy val ledger: Ledger = newLedger()

  override lazy val stxLedger: StxLedger = new StxLedger(blockchain, blockchainConfig, consensus.blockPreparator)
}

trait CheckpointBlockGeneratorBuilder {
  lazy val checkpointBlockGenerator = new CheckpointBlockGenerator()
}

trait SyncControllerBuilder {

  self: ActorSystemBuilder
    with ServerActorBuilder
    with BlockchainBuilder
    with NodeStatusBuilder
    with StorageBuilder
    with LedgerBuilder
    with PeerEventBusBuilder
    with PendingTransactionsManagerBuilder
    with OmmersPoolBuilder
    with EtcPeerManagerActorBuilder
    with SyncConfigBuilder
    with ShutdownHookBuilder
    with ConsensusBuilder
    with BlacklistBuilder =>

  lazy val syncController: ActorRef = system.actorOf(
    SyncController.props(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      consensus.validators,
      peerEventBus,
      pendingTransactionsManager,
      ommersPool,
      etcPeerManager,
      blacklist,
      syncConfig
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
    override def run(): Unit = {
      shutdown()
    }
  })

  def shutdownOnError[A](f: => A): A = {
    Try(f) match {
      case Success(v) => v
      case Failure(t) =>
        log.error(t.getMessage, t)
        shutdown()
        throw t
    }
  }
}

object ShutdownHookBuilder extends ShutdownHookBuilder with Logger

trait GenesisDataLoaderBuilder {
  self: BlockchainBuilder with StorageBuilder with BlockchainConfigBuilder =>

  lazy val genesisDataLoader = new GenesisDataLoader(blockchain, blockchainConfig)
}

/** Provides the basic functionality of a Node, except the consensus algorithm.
  * The latter is loaded dynamically based on configuration.
  *
  * @see [[io.iohk.ethereum.consensus.ConsensusBuilder ConsensusBuilder]],
  *      [[io.iohk.ethereum.consensus.ConsensusConfigBuilder ConsensusConfigBuilder]]
  */
trait Node
    extends SecureRandomBuilder
    with NodeKeyBuilder
    with ActorSystemBuilder
    with StorageBuilder
    with BlockchainBuilder
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
    with ConsensusBuilder
    with ConsensusConfigBuilder
    with LedgerBuilder
    with KeyStoreConfigBuilder
    with AsyncConfigBuilder
    with CheckpointBlockGeneratorBuilder
    with TransactionHistoryServiceBuilder.Default
    with PortForwardingBuilder
    with BlacklistBuilder
