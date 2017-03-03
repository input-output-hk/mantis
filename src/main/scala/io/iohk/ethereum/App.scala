package io.iohk.ethereum

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.agent._
import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, _}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.PeerManagerActor.PeersResponse
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.rpc.JsonRpcServer
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

object App {

  val nodeKey = generateKeyPair()

  val storagesInstance: DataSourcesComponent with StoragesComponent = new SharedLevelDBDataSources with Storages.DefaultStorages
  val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")

    val appActor = actorSystem.actorOf(AppActor.props(nodeKey, blockchain, storagesInstance, actorSystem), "app")

    appActor ! AppActor.StartApp
  }

}

class AppActor(nodeKey: AsymmetricCipherKeyPair,
               blockchain: Blockchain,
               storagesInstance: DataSourcesComponent with StoragesComponent,
               actorSystem: ActorSystem) extends Actor {

  import Config.{Network => NetworkConfig}
  import AppActor._

  override def receive: Receive = idle

  def idle: Receive = {

    case StartApp =>
      val nodeStatus =
        NodeStatus(
          key = nodeKey,
          serverStatus = ServerStatus.NotListening,
          blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash, 0))

      val nodeStatusHolder = Agent(nodeStatus)

      val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeStatusHolder, Config.Network.peer, blockchain), "peer-manager")
      val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

      server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

      if(Config.Network.Rpc.enabled) JsonRpcServer.run(actorSystem, blockchain, Config.Network.Rpc)

      val fastSyncController = actorSystem.actorOf(
        FastSyncController.props(
          peerManager,
          nodeStatusHolder,
          blockchain,
          storagesInstance.storages.mptNodeStorage),
        "fast-sync-controller")

      fastSyncController ! FastSyncController.StartFastSync

      Runtime.getRuntime.addShutdownHook(new Thread() {
        override def run(): Unit = {
          storagesInstance.dataSources.closeAll
        }
      })

      context become waitingForFastSyncDone(blockchain, peerManager)

  }

  def waitingForFastSyncDone(blockchain: Blockchain, peerManager: ActorRef): Receive = {
    case FastSyncController.FastSyncDone =>
      //Ask for peers to start block broadcast
      peerManager ! PeerManagerActor.GetPeers

    case PeersResponse(peers) =>
      peers.foreach{ peer =>
        peer.ref ! PeerActor.StartBlockBroadcast
      }
  }

}

object AppActor {
  def props(nodeKey: AsymmetricCipherKeyPair, blockchain: Blockchain,
            storagesInstance: DataSourcesComponent with StoragesComponent, actorSystem: ActorSystem): Props = {
    Props(new AppActor(nodeKey, blockchain, storagesInstance, actorSystem))
  }

  case object StartApp
}
