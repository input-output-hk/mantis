package io.iohk.ethereum.nodebuilder

import akka.actor.ActorSystem
import akka.agent.Agent
import io.iohk.ethereum.blockchain.sync.{SyncController}
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.rpc.{JsonRpcServer, RpcServerConfig}
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.p2p.validators.BlockValidator


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
    with BlockChainBuilder =>

  lazy val peerConfiguration = Config.Network.peer

  lazy val peerManager = actorSystem.actorOf(PeerManagerActor.props(
    nodeStatusHolder,
    Config.Network.peer,
    storagesInstance.storages.appStateStorage,
    blockchain), "peer-manager")

}

trait ServerActorBuilder {

  self: ActorSystemBuilder
    with NodeStatusBuilder
    with BlockChainBuilder
    with PeerManagerActorBuilder =>

  lazy val networkConfig = Config.Network

  lazy val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

}


trait JSONRpcServerBuilder {

  self: ActorSystemBuilder with BlockChainBuilder =>

  def startJSONRpcServer(): Unit = JsonRpcServer.run(actorSystem, blockchain, Config.Network.Rpc)

  lazy val rpcServerConfig: RpcServerConfig = Config.Network.Rpc

}

trait FastSyncControllerBuilder {

  self: ActorSystemBuilder with
    ServerActorBuilder with
    BlockChainBuilder with
    NodeStatusBuilder with
    PeerManagerActorBuilder with
    StorageBuilder =>

  lazy val syncController = actorSystem.actorOf(
    SyncController.props(
      peerManager,
      nodeStatusHolder,
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages.mptNodeStorage,
      storagesInstance.storages.fastSyncStateStorage,
      BlockValidator.validateHeaderAndBody),
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

trait Node extends NodeKeyBuilder
  with ActorSystemBuilder
  with StorageBuilder
  with BlockChainBuilder
  with NodeStatusBuilder
  with PeerManagerActorBuilder
  with ServerActorBuilder
  with FastSyncControllerBuilder
  with JSONRpcServerBuilder
  with ShutdownHookBuilder
