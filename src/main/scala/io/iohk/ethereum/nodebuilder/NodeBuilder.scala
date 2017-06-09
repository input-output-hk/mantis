package io.iohk.ethereum.nodebuilder

import akka.actor.{ActorRef, ActorSystem}
import akka.agent.Agent
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.{BlockchainHostActor, SyncController}
import io.iohk.ethereum.blockchain.sync.SyncController.DependencyActors
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.keystore.{KeyStore, KeyStoreImpl}
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.PeersInfoHolderActor.PeerInfo
import io.iohk.ethereum.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM
import io.iohk.ethereum.ommers.OmmersPool

trait BlockchainConfigBuilder {
  lazy val blockchainConfig = BlockchainConfig(Config.config)
}

trait MiningConfigBuilder {
  lazy val miningConfig = MiningConfig(Config.config)
}

trait NodeKeyBuilder {
  lazy val nodeKey = loadAsymmetricCipherKeyPair(Config.keysFile)
}

trait ActorSystemBuilder {
  implicit lazy val actorSystem = ActorSystem("etc-client_system")
}

trait StorageBuilder {
  lazy val storagesInstance =  new SharedLevelDBDataSources with Storages.DefaultStorages
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

trait PeerEventBusBuilder {
  self: ActorSystemBuilder =>

  lazy val peerEventBus = actorSystem.actorOf(PeerEventBusActor.props)
}

trait PeerManagerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with HandshakerBuilder
    with PeerEventBusBuilder =>

  lazy val peerConfiguration = Config.Network.peer

  lazy val peerManager = actorSystem.actorOf(PeerManagerActor.props(
    nodeStatusHolder,
    Config.Network.peer,
    peerEventBus,
    handshaker), "peer-manager")

}

trait PeersInfoHolderBuilder {
  self: ActorSystemBuilder
    with PeerEventBusBuilder
    with ForkResolverBuilder
    with StorageBuilder =>

  lazy val peersInfoHolder = actorSystem.actorOf(PeersInfoHolderActor.props(
    peerEventBus, storagesInstance.storages.appStateStorage, forkResolverOpt))

}

trait BlockchainHostBuilder {
  self: ActorSystemBuilder
    with BlockChainBuilder
    with PeerManagerActorBuilder
    with PeerEventBusBuilder =>

  val blockchainHost = actorSystem.actorOf(BlockchainHostActor.props(
    blockchain, peerConfiguration, peerEventBus, peerManager))

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
    with PeerEventBusBuilder
    with MiningConfigBuilder =>

  lazy val pendingTransactionsManager: ActorRef = actorSystem.actorOf(PendingTransactionsManager.props(miningConfig, peerManager, peerEventBus))
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
    PendingTransactionsManagerBuilder with
    LedgerBuilder with
    ValidatorsBuilder with
    BlockchainConfigBuilder with
    KeyStoreBuilder with
    SyncControllerBuilder with
    OmmersPoolBuilder with
    MiningConfigBuilder =>

  lazy val ethService = new EthService(storagesInstance.storages, blockGenerator, storagesInstance.storages.appStateStorage, miningConfig,
    ledger, blockchainConfig, keyStore, pendingTransactionsManager, syncController, ommersPool)
}

trait PersonalServiceBuilder {
  self: KeyStoreBuilder with BlockChainBuilder with PendingTransactionsManagerBuilder with StorageBuilder =>

  lazy val personalService = new PersonalService(keyStore, blockchain, pendingTransactionsManager,
    storagesInstance.storages, storagesInstance.storages.appStateStorage)
}

trait KeyStoreBuilder {
  lazy val keyStore: KeyStore = new KeyStoreImpl(Config.keyStoreDir)
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

  val validators = new Validators {
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
    PeersInfoHolderBuilder =>



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
      peersInfoHolder), "sync-controller")

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

  lazy val genesisDataLoader = new GenesisDataLoader(storagesInstance.dataSource, blockchain, blockchainConfig)
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
  with PeersInfoHolderBuilder
  with BlockchainHostBuilder
