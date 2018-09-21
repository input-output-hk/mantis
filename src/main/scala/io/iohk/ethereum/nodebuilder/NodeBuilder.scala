package io.iohk.ethereum.nodebuilder

import java.security.SecureRandom
import java.time.Clock

import akka.actor.{ActorRef, ActorSystem}
import akka.agent.Agent
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.{BlockchainHostActor, SyncController}
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{DataSourcesComponent, SharedLevelDBDataSources, Storages, StoragesComponent}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.NetService.NetServiceConfig
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer
import io.iohk.ethereum.jsonrpc.server.websocket.JsonRpcWebsocketServer
import io.iohk.ethereum.keystore.{KeyStore, KeyStoreImpl}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, DiscoveryListener, PeerDiscoveryManager}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor, _}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.testmode.{TestLedgerBuilder, TestmodeConsensusBuilder}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

// scalastyle:off number.of.types
trait BlockchainConfigBuilder {
  lazy val blockchainConfig = BlockchainConfig(Config.config)
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

trait NodeKeyBuilder {
  self: SecureRandomBuilder =>
  lazy val nodeKey = loadAsymmetricCipherKeyPair(Config.nodeKeyFile, secureRandom)
}

trait ActorSystemBuilder {
  implicit lazy val system = ActorSystem("mantis_system")
}

trait PruningConfigBuilder extends PruningModeComponent {
  lazy val pruningMode: PruningMode = PruningConfig(Config.config).mode
}

trait StorageBuilder {
  lazy val storagesInstance: DataSourcesComponent with StoragesComponent with PruningModeComponent with BlockchainConfigBuilder  =
    new SharedLevelDBDataSources with PruningConfigBuilder with Storages.DefaultStorages with BlockchainConfigBuilder
}

trait DiscoveryConfigBuilder {
  lazy val discoveryConfig = DiscoveryConfig(Config.config)
}

trait KnownNodesManagerBuilder {
  self: ActorSystemBuilder
    with StorageBuilder =>

  lazy val knownNodesManagerConfig = KnownNodesManager.KnownNodesManagerConfig(Config.config)

  lazy val knownNodesManager = system.actorOf(
    KnownNodesManager.props(
      knownNodesManagerConfig,
      storagesInstance.storages.knownNodesStorage
    ),
    "known-nodes-manager"
  )
}

trait PeerDiscoveryManagerBuilder {
  self: ActorSystemBuilder
  with DiscoveryListenerBuilder
  with NodeStatusBuilder
  with DiscoveryConfigBuilder
  with StorageBuilder =>

  lazy val peerDiscoveryManager =
    system.actorOf(PeerDiscoveryManager.props(discoveryListener, discoveryConfig,
      storagesInstance.storages.knownNodesStorage, nodeStatusHolder, Clock.systemUTC()), "peer-discovery-manager")
}

trait DiscoveryListenerBuilder {
  self: ActorSystemBuilder
  with DiscoveryConfigBuilder
  with NodeStatusBuilder =>

  lazy val discoveryListener = system.actorOf(DiscoveryListener.props(discoveryConfig, nodeStatusHolder), "discovery-listener")
}

trait NodeStatusBuilder {

  self : NodeKeyBuilder =>

  private val nodeStatus =
    NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening)

  lazy val nodeStatusHolder = Agent(nodeStatus)
}

trait BlockchainBuilder {
  self: StorageBuilder =>

  lazy val blockchain: BlockchainImpl = BlockchainImpl(storagesInstance.storages)
}

trait ForkResolverBuilder {
  self: BlockchainConfigBuilder =>

  lazy val forkResolverOpt = blockchainConfig.daoForkConfig.map(new ForkResolver.EtcForkResolver(_))

}

trait HandshakerBuilder {
  self: BlockchainBuilder
    with NodeStatusBuilder
    with StorageBuilder
    with PeerManagerActorBuilder
    with BlockchainConfigBuilder
    with ForkResolverBuilder =>

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = self.forkResolverOpt
      override val nodeStatusHolder: Agent[NodeStatus] = self.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = self.peerConfiguration
      override val blockchain: Blockchain = self.blockchain
      override val appStateStorage: AppStateStorage = self.storagesInstance.storages.appStateStorage
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)
}

trait AuthHandshakerBuilder {
  self: NodeKeyBuilder
  with SecureRandomBuilder =>

  lazy val authHandshaker: AuthHandshaker = AuthHandshaker(nodeKey, secureRandom)
}

trait PeerEventBusBuilder {
  self: ActorSystemBuilder =>

  lazy val peerEventBus = system.actorOf(PeerEventBusActor.props, "peer-event-bus")
}

trait PeerManagerActorBuilder {

  self: ActorSystemBuilder
    with HandshakerBuilder
    with PeerEventBusBuilder
    with AuthHandshakerBuilder
    with PeerDiscoveryManagerBuilder
    with StorageBuilder
    with KnownNodesManagerBuilder
    with BlockchainConfigBuilder =>

  lazy val peerConfiguration = Config.Network.peer

  lazy val peerManager = system.actorOf(PeerManagerActor.props(
    peerDiscoveryManager,
    Config.Network.peer,
    peerEventBus,
    knownNodesManager,
    handshaker,
    authHandshaker,
    EthereumMessageDecoder(blockchainConfig.ethCompatibilityMode)), "peer-manager")

}

trait EtcPeerManagerActorBuilder {
  self: ActorSystemBuilder
    with PeerManagerActorBuilder
    with PeerEventBusBuilder
    with ForkResolverBuilder
    with StorageBuilder =>

  lazy val etcPeerManager = system.actorOf(EtcPeerManagerActor.props(
    peerManager, peerEventBus, storagesInstance.storages.appStateStorage, forkResolverOpt), "etc-peer-manager")

}

trait BlockchainHostBuilder {
  self: ActorSystemBuilder
    with BlockchainBuilder
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder =>

  val blockchainHost = system.actorOf(BlockchainHostActor.props(
    blockchain, peerConfiguration, peerEventBus, etcPeerManager), "blockchain-host")

}

trait ServerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with BlockchainBuilder
    with PeerManagerActorBuilder =>

  lazy val networkConfig = Config.Network

  lazy val server = system.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

}

trait Web3ServiceBuilder {
  self: VmConfigBuilder =>

  lazy val web3Service = new Web3Service(vmConfig)
}

trait NetServiceBuilder {
  this: PeerManagerActorBuilder with NodeStatusBuilder =>

  lazy val netServiceConfig = NetServiceConfig(Config.config)

  lazy val netService = new NetService(nodeStatusHolder, peerManager, netServiceConfig)
}

trait PendingTransactionsManagerBuilder {
  self: ActorSystemBuilder
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder
    with TxPoolConfigBuilder =>

  lazy val pendingTransactionsManager: ActorRef = system.actorOf(PendingTransactionsManager.props(
    txPoolConfig, peerManager, etcPeerManager, peerEventBus))
}

trait FilterManagerBuilder {
  self: ActorSystemBuilder
    with BlockchainBuilder
    with BlockGeneratorBuilder
    with StorageBuilder
    with KeyStoreBuilder
    with PendingTransactionsManagerBuilder
    with FilterConfigBuilder
    with TxPoolConfigBuilder =>

  lazy val filterManager: ActorRef =
    system.actorOf(
      FilterManager.props(
        blockchain,
        blockGenerator, // FIXME get from consensus
        storagesInstance.storages.appStateStorage,
        keyStore,
        pendingTransactionsManager,
        filterConfig,
        txPoolConfig), "filter-manager")
}

// FIXME Remove, since the consensus provides this now
trait BlockGeneratorBuilder {
  self: ConsensusBuilder with
        BlockchainConfigBuilder with
        ConsensusConfigBuilder with
        Logger ⇒

  lazy val blockGenerator = consensus.blockGenerator
}

trait TestServiceBuilder {
  self: BlockchainBuilder with
    PendingTransactionsManagerBuilder with
    ConsensusConfigBuilder with
    BlockchainConfigBuilder with
    VmBuilder with
    TestmodeConsensusBuilder with
    TestLedgerBuilder =>

  lazy val testService = new TestService(blockchain, pendingTransactionsManager, consensusConfig, consensus, testLedgerWrapper)
}

trait EthServiceBuilder {
  self: StorageBuilder with
    BlockchainBuilder with
    BlockGeneratorBuilder with
    BlockchainConfigBuilder with
    PendingTransactionsManagerBuilder with
    LedgerBuilder with
    KeyStoreBuilder with
    SyncControllerBuilder with
    OmmersPoolBuilder with
    ConsensusBuilder with
    ConsensusConfigBuilder with
    FilterManagerBuilder with
    FilterConfigBuilder with
    TxPoolConfigBuilder with
    VmConfigBuilder with
    JSONRpcConfigBuilder =>

  lazy val ethService = new EthService(blockchain, storagesInstance.storages.appStateStorage,
    ledger, keyStore, pendingTransactionsManager, syncController, ommersPool, filterManager, filterConfig,
    blockchainConfig, Config.Network.protocolVersion, jsonRpcConfig, vmConfig,
    txPoolConfig.getTransactionFromPoolTimeout)
}

trait PersonalServiceBuilder {
  self: KeyStoreBuilder with
    BlockchainBuilder with
    BlockchainConfigBuilder with
    PendingTransactionsManagerBuilder with
    StorageBuilder with
    TxPoolConfigBuilder =>

  lazy val personalService = new PersonalService(keyStore, blockchain, pendingTransactionsManager,
    storagesInstance.storages.appStateStorage, blockchainConfig, txPoolConfig)
}

trait KeyStoreBuilder {
  self: SecureRandomBuilder =>
  lazy val keyStore: KeyStore = new KeyStoreImpl(Config.keyStoreDir, secureRandom)
}

trait JSONRpcConfigBuilder {
  lazy val jsonRpcConfig: JsonRpcConfig = JsonRpcConfig(Config.config)
}

trait JSONRpcControllerBuilder {
  this: Web3ServiceBuilder with
    EthServiceBuilder with
    NetServiceBuilder with
    PersonalServiceBuilder with
    JSONRpcConfigBuilder =>

  private val testService =
    if (Config.testmode) Some(this.asInstanceOf[TestServiceBuilder].testService)
    else None

  lazy val jsonRpcController = new JsonRpcController(web3Service, netService, ethService, personalService, testService, jsonRpcConfig)
}

trait JSONRpcHttpServerBuilder {
  self: ActorSystemBuilder with JSONRpcControllerBuilder with SecureRandomBuilder with JSONRpcConfigBuilder =>

  lazy val maybeJsonRpcHttpServer = JsonRpcHttpServer(jsonRpcController, jsonRpcConfig.httpServerConfig, secureRandom)
}

trait JSONRpcWebsocketServerBuilder {
  self: ActorSystemBuilder with JSONRpcControllerBuilder with JSONRpcConfigBuilder with BlockchainBuilder =>

  lazy val jsonRpcWebsocketServer = new JsonRpcWebsocketServer(jsonRpcController, blockchain, jsonRpcConfig.websocketServerConfig)
}

trait JSONRpcIpcServerBuilder {
  self: ActorSystemBuilder with JSONRpcControllerBuilder with JSONRpcConfigBuilder =>

  lazy val jsonRpcIpcServer = new JsonRpcIpcServer(jsonRpcController, jsonRpcConfig.ipcServerConfig)
}

trait OmmersPoolBuilder {
  self: ActorSystemBuilder with
    BlockchainBuilder with
    ConsensusConfigBuilder =>

  lazy val ommersPoolSize: Int = 30 // FIXME For this we need EthashConfig, which means Ethash consensus
  lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(blockchain, ommersPoolSize))
}

trait VmBuilder {
  self: ActorSystemBuilder
    with BlockchainConfigBuilder
    with VmConfigBuilder =>

  lazy val vm: VMImpl = VmSetup.vm(vmConfig, blockchainConfig, testMode = false)
}

trait LedgerBuilder {
  def ledger: Ledger
}

trait StdLedgerBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder =>

  // This is used in tests, which need the more specific type
  // Note See if the APIs that the tests need can be promoted to the Ledger interface.
  // Note In fact, most if all these APIs are now being delegated to the BlockPreparator,
  //      so a refactoring should probably take that into account.
  protected def newLedger(): LedgerImpl = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus)

  override lazy val ledger: Ledger = newLedger()
}

trait SyncControllerBuilder {

  self: ActorSystemBuilder with
    ServerActorBuilder with
    BlockchainBuilder with
    NodeStatusBuilder with
    StorageBuilder with
    BlockchainConfigBuilder with
    LedgerBuilder with
    PeerEventBusBuilder with
    PendingTransactionsManagerBuilder with
    OmmersPoolBuilder with
    EtcPeerManagerActorBuilder with
    SyncConfigBuilder with
    ShutdownHookBuilder with
    ConsensusBuilder =>

  lazy val syncController = system.actorOf(
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
      syncConfig,
      () => shutdown()
    ), "sync-controller")

}

trait ShutdownHookBuilder { self: Logger ⇒
  def shutdown(): Unit = {/* No default behaviour during shutdown. */}

  lazy val shutdownTimeoutDuration = Config.shutdownTimeout

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      shutdown()
    }
  })

  def shutdownOnError[A](f: ⇒ A): A = {
    Try(f) match {
      case Success(v) ⇒ v
      case Failure(t) ⇒
        log.error(t.getMessage, t)
        shutdown()
        throw t
    }
  }
}

object ShutdownHookBuilder extends ShutdownHookBuilder with Logger

trait GenesisDataLoaderBuilder {
  self: BlockchainBuilder
    with StorageBuilder
    with BlockchainConfigBuilder =>

  lazy val genesisDataLoader = new GenesisDataLoader(blockchain, blockchainConfig)
}

trait SecureRandomBuilder {
  lazy val secureRandom: SecureRandom =
    Config.secureRandomAlgo.map(SecureRandom.getInstance).getOrElse(new SecureRandom())
}

/**
 * Provides the basic functionality of a Node, except the consensus algorithm.
 * The latter is loaded dynamically based on configuration.
 *
 * @see [[io.iohk.ethereum.consensus.ConsensusBuilder ConsensusBuilder]],
 *      [[io.iohk.ethereum.consensus.ConsensusConfigBuilder ConsensusConfigBuilder]]
 */
trait Node extends NodeKeyBuilder
  with ActorSystemBuilder
  with StorageBuilder
  with BlockchainBuilder
  with NodeStatusBuilder
  with ForkResolverBuilder
  with HandshakerBuilder
  with PeerManagerActorBuilder
  with ServerActorBuilder
  with SyncControllerBuilder
  with Web3ServiceBuilder
  with EthServiceBuilder
  with NetServiceBuilder
  with PersonalServiceBuilder
  with KeyStoreBuilder
  with BlockGeneratorBuilder
  with JSONRpcConfigBuilder
  with JSONRpcControllerBuilder
  with JSONRpcHttpServerBuilder
  with JSONRpcIpcServerBuilder
  with JSONRpcWebsocketServerBuilder
  with ShutdownHookBuilder
  with Logger
  with GenesisDataLoaderBuilder
  with BlockchainConfigBuilder
  with VmConfigBuilder
  with PeerEventBusBuilder
  with PendingTransactionsManagerBuilder
  with OmmersPoolBuilder
  with EtcPeerManagerActorBuilder
  with BlockchainHostBuilder
  with FilterManagerBuilder
  with FilterConfigBuilder
  with TxPoolConfigBuilder
  with SecureRandomBuilder
  with AuthHandshakerBuilder
  with PruningConfigBuilder
  with PeerDiscoveryManagerBuilder
  with DiscoveryConfigBuilder
  with DiscoveryListenerBuilder
  with KnownNodesManagerBuilder
  with SyncConfigBuilder
  with VmBuilder
  with ConsensusBuilder
  with ConsensusConfigBuilder
  with LedgerBuilder
