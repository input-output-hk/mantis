package io.iohk.ethereum.nodebuilder

import akka.actor.ActorSystem
import akka.agent.Agent
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.jsonrpc.{EthService, JsonRpcController, Web3Service}
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global
import io.iohk.ethereum.network._
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM

trait BlockchainConfigBuilder {
  lazy val blockchainConfig = BlockchainConfig(Config.config)
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


trait PeerManagerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with StorageBuilder
    with BlockChainBuilder
    with BlockchainConfigBuilder =>

  lazy val peerConfiguration = Config.Network.peer

  lazy val peerManager = actorSystem.actorOf(PeerManagerActor.props(
    nodeStatusHolder,
    Config.Network.peer,
    storagesInstance.storages.appStateStorage,
    blockchain,
    blockchainConfig), "peer-manager")

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

trait EthServiceBuilder {

  self: BlockChainBuilder =>

  lazy val ethService = new EthService(blockchain)
}

trait JSONRpcControllerBuilder {
  this: Web3ServiceBuilder with EthServiceBuilder =>

  lazy val jsonRpcController = new JsonRpcController(web3Service, ethService)
}

trait JSONRpcHttpServerBuilder {

  self: ActorSystemBuilder with BlockChainBuilder with JSONRpcControllerBuilder =>

  lazy val jsonRpcHttpServerConfig: JsonRpcHttpServerConfig = Config.Network.Rpc

  lazy val jsonRpcHttpServer = new JsonRpcHttpServer(jsonRpcController, jsonRpcHttpServerConfig)
}

trait SyncControllerBuilder {

  self: ActorSystemBuilder with
    ServerActorBuilder with
    BlockChainBuilder with
    NodeStatusBuilder with
    PeerManagerActorBuilder with
    StorageBuilder with
    BlockchainConfigBuilder =>

  val validators = new Validators {
    val blockValidator: BlockValidator = BlockValidator
    val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
    val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
    val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
  }

  val ledger: Ledger = new LedgerImpl(VM, blockchainConfig)

  lazy val syncController = actorSystem.actorOf(
    SyncController.props(
      peerManager,
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      validators),
    "sync-controller")

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
  with PeerManagerActorBuilder
  with ServerActorBuilder
  with SyncControllerBuilder
  with Web3ServiceBuilder
  with EthServiceBuilder
  with JSONRpcControllerBuilder
  with JSONRpcHttpServerBuilder
  with ShutdownHookBuilder
  with GenesisDataLoaderBuilder
  with BlockchainConfigBuilder
