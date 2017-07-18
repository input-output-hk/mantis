package io.iohk.ethereum.nodebuilder

import java.security.SecureRandom

import akka.actor.{ActorRef, ActorSystem}
import akka.agent.Agent
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.{BlockchainHostActor, SyncController}
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.keystore.{KeyStore, KeyStoreImpl}
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.utils.Config.DbConfig

trait BlockchainConfigBuilder {
  lazy val blockchainConfig = BlockchainConfig(Config.config)
}

trait TxPoolConfigBuilder {
  lazy val txPoolConfig = TxPoolConfig(Config.config)
}

trait MiningConfigBuilder {
  lazy val miningConfig = MiningConfig(Config.config)
}

trait FilterConfigBuilder {
  lazy val filterConfig = FilterConfig(Config.config)
}

trait NodeKeyBuilder {
  self: SecureRandomBuilder =>
  lazy val nodeKey = loadAsymmetricCipherKeyPair(Config.nodeKeyFile, secureRandom)
}

trait ActorSystemBuilder {
  implicit lazy val actorSystem = ActorSystem("etc-client_system")
}

trait PruningConfigBuilder {
  lazy val pruningConfig = PruningConfig(Config.config)
  lazy val pruningMode: PruningMode = pruningConfig.mode
}

trait StorageBuilder {
  lazy val storagesInstance =  new SharedLevelDBDataSources with PruningConfigBuilder with Storages.DefaultStorages
}

trait NodeStatusBuilder {

  self : NodeKeyBuilder =>

  private val nodeStatus =
    NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening)

  lazy val nodeStatusHolder = Agent(nodeStatus)
}

trait BlockChainBuilder {
  self: StorageBuilder =>

  lazy val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)
}

trait ForkResolverBuilder {
  self: BlockchainConfigBuilder =>

  lazy val forkResolverOpt =
    if (blockchainConfig.customGenesisFileOpt.isDefined) None
    else Some(new ForkResolver.EtcForkResolver(blockchainConfig))
}

trait HandshakerBuilder {
  self: BlockChainBuilder
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

  lazy val peerEventBus = actorSystem.actorOf(PeerEventBusActor.props, "peer-event-bus")
}

trait PeerManagerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with HandshakerBuilder
    with PeerEventBusBuilder
    with AuthHandshakerBuilder =>

  lazy val peerConfiguration = Config.Network.peer

  lazy val peerManager = actorSystem.actorOf(PeerManagerActor.props(
    nodeStatusHolder,
    Config.Network.peer,
    peerEventBus,
    handshaker,
    authHandshaker,
    EthereumMessageDecoder), "peer-manager")

}

trait EtcPeerManagerActorBuilder {
  self: ActorSystemBuilder
    with PeerManagerActorBuilder
    with PeerEventBusBuilder
    with ForkResolverBuilder
    with StorageBuilder =>

  lazy val etcPeerManager = actorSystem.actorOf(EtcPeerManagerActor.props(
    peerManager, peerEventBus, storagesInstance.storages.appStateStorage, forkResolverOpt), "etc-peer-manager")

}

trait BlockchainHostBuilder {
  self: ActorSystemBuilder
    with BlockChainBuilder
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder =>

  val blockchainHost = actorSystem.actorOf(BlockchainHostActor.props(
    blockchain, peerConfiguration, peerEventBus, etcPeerManager), "blockchain-host")

}

trait ServerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with BlockChainBuilder
    with PeerManagerActorBuilder =>

  lazy val networkConfig = Config.Network

  lazy val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

}

trait Web3ServiceBuilder {
  lazy val web3Service = new Web3Service
}

trait NetServiceBuilder {
  this: PeerManagerActorBuilder with NodeStatusBuilder =>

  lazy val netService = new NetService(nodeStatusHolder, peerManager)
}

trait PendingTransactionsManagerBuilder {
  self: ActorSystemBuilder
    with PeerManagerActorBuilder
    with EtcPeerManagerActorBuilder
    with PeerEventBusBuilder
    with TxPoolConfigBuilder =>

  lazy val pendingTransactionsManager: ActorRef = actorSystem.actorOf(PendingTransactionsManager.props(
    txPoolConfig, peerManager, etcPeerManager, peerEventBus))
}

trait FilterManagerBuilder {
  self: ActorSystemBuilder
    with BlockChainBuilder
    with BlockGeneratorBuilder
    with StorageBuilder
    with KeyStoreBuilder
    with PendingTransactionsManagerBuilder
    with FilterConfigBuilder
    with TxPoolConfigBuilder =>

  lazy val filterManager: ActorRef =
    actorSystem.actorOf(
      FilterManager.props(
        blockchain,
        blockGenerator,
        storagesInstance.storages.appStateStorage,
        keyStore,
        pendingTransactionsManager,
        filterConfig,
        txPoolConfig
      )
    )
}

trait BlockGeneratorBuilder {
  self: StorageBuilder with
    BlockchainConfigBuilder with
    ValidatorsBuilder with
    LedgerBuilder with
    MiningConfigBuilder =>

  lazy val blockGenerator = new BlockGenerator(storagesInstance.storages, blockchainConfig, miningConfig, ledger, validators)
}

trait EthServiceBuilder {
  self: StorageBuilder with
    BlockChainBuilder with
    BlockGeneratorBuilder with
    BlockchainConfigBuilder with
    PendingTransactionsManagerBuilder with
    LedgerBuilder with
    ValidatorsBuilder with
    BlockchainConfigBuilder with
    KeyStoreBuilder with
    SyncControllerBuilder with
    OmmersPoolBuilder with
    MiningConfigBuilder with
    TxPoolConfigBuilder with
    FilterManagerBuilder with
    FilterConfigBuilder =>

  lazy val ethService = new EthService(storagesInstance.storages, blockGenerator, storagesInstance.storages.appStateStorage, miningConfig,
    txPoolConfig, ledger, keyStore, pendingTransactionsManager, syncController, ommersPool, filterManager, filterConfig, blockchainConfig)
}

trait PersonalServiceBuilder {
  self: KeyStoreBuilder with
    BlockChainBuilder with
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

trait JSONRpcControllerBuilder {
  this: Web3ServiceBuilder with EthServiceBuilder with NetServiceBuilder with PersonalServiceBuilder =>

  lazy val jsonRpcController = new JsonRpcController(web3Service, netService, ethService, personalService, Config.Network.Rpc)
}

trait JSONRpcHttpServerBuilder {

  self: ActorSystemBuilder with BlockChainBuilder with JSONRpcControllerBuilder =>

  lazy val jsonRpcHttpServerConfig: JsonRpcHttpServerConfig = Config.Network.Rpc

  lazy val jsonRpcHttpServer = new JsonRpcHttpServer(jsonRpcController, jsonRpcHttpServerConfig)
}

trait OmmersPoolBuilder {
  self: ActorSystemBuilder with
    BlockChainBuilder with
    MiningConfigBuilder =>

  lazy val ommersPool: ActorRef = actorSystem.actorOf(OmmersPool.props(blockchain, miningConfig))
}

trait ValidatorsBuilder {
  self: BlockchainConfigBuilder =>

  lazy val validators = new Validators {
    val blockValidator: BlockValidator = BlockValidator
    val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
    val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
    val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
  }
}

trait LedgerBuilder {
  self: BlockchainConfigBuilder =>

  lazy val ledger: Ledger = new LedgerImpl(VM, blockchainConfig)
}

trait SyncControllerBuilder {

  self: ActorSystemBuilder with
    ServerActorBuilder with
    BlockChainBuilder with
    NodeStatusBuilder with
    StorageBuilder with
    BlockchainConfigBuilder with
    ValidatorsBuilder with
    LedgerBuilder with
    PeerEventBusBuilder with
    PendingTransactionsManagerBuilder with
    OmmersPoolBuilder with
    EtcPeerManagerActorBuilder =>



  lazy val syncController = actorSystem.actorOf(
    SyncController.props(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      validators,
      peerEventBus,
      pendingTransactionsManager,
      ommersPool,
      etcPeerManager), "sync-controller")

}

trait ShutdownHookBuilder {

  def shutdown(): Unit

  lazy val shutdownTimeoutDuration = Config.shutdownTimeout

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = {
      shutdown()
    }
  })
}

trait GenesisDataLoaderBuilder {
  self: BlockChainBuilder
    with StorageBuilder
    with BlockchainConfigBuilder =>

  lazy val genesisDataLoader = new GenesisDataLoader(storagesInstance.dataSource, blockchain, storagesInstance.pruningMode, blockchainConfig, Config.Db)
}

trait SecureRandomBuilder {
  lazy val secureRandom: SecureRandom = SecureRandom.getInstance(Config.secureRandomAlgo)
}

trait Node extends NodeKeyBuilder
  with ActorSystemBuilder
  with StorageBuilder
  with BlockChainBuilder
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
  with ValidatorsBuilder
  with LedgerBuilder
  with JSONRpcControllerBuilder
  with JSONRpcHttpServerBuilder
  with ShutdownHookBuilder
  with GenesisDataLoaderBuilder
  with BlockchainConfigBuilder
  with PeerEventBusBuilder
  with PendingTransactionsManagerBuilder
  with OmmersPoolBuilder
  with MiningConfigBuilder
  with EtcPeerManagerActorBuilder
  with BlockchainHostBuilder
  with FilterManagerBuilder
  with FilterConfigBuilder
  with TxPoolConfigBuilder
  with SecureRandomBuilder
  with AuthHandshakerBuilder
  with PruningConfigBuilder
