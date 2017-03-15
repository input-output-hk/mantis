package io.iohk.ethereum

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.agent._
import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, _}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.PeerManagerActor.PeersResponse
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.rpc.JsonRpcServer
import io.iohk.ethereum.utils.{Config, Logger, NodeStatus, ServerStatus}
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.p2p.validators.BlockValidator
import io.iohk.ethereum.nodebuilder.Node
import org.spongycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey: AsymmetricCipherKeyPair = loadAsymmetricCipherKeyPair(Config.keysFile)

  val storagesInstance: DataSourcesComponent with StoragesComponent = new SharedLevelDBDataSources with Storages.DefaultStorages
  val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)

  def main(args: Array[String]): Unit = {

    new Node with Logger {

      def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
        case Failure(e) => log.warn("Error while shutting down...", e)
        case Success(_) =>
      }

      override def shutdown(): Unit = {
        tryAndLogFailure(() => storagesInstance.dataSources.closeAll)
        tryAndLogFailure(() => Await.ready(actorSystem.terminate, shutdownTimeoutDuration))
      }

      server ! ServerActor.StartServer(networkConfig.Server.listenAddress)
      fastSyncController ! SyncController.StartSync

      if(rpcServerConfig.enabled) startJSONRpcServer()
    }

    /*val actorSystem = ActorSystem("etc-client_system")

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(
      nodeStatusHolder,
      Config.Network.peer,
      storagesInstance.storages.appStateStorage,
      blockchain), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    if (Config.Network.Rpc.enabled) JsonRpcServer.run(actorSystem, blockchain, Config.Network.Rpc)

    val fastSyncController = actorSystem.actorOf(
      SyncController.props(
        peerManager,
        nodeStatusHolder,
        storagesInstance.storages.appStateStorage,
        blockchain,
        storagesInstance.storages.mptNodeStorage,
        storagesInstance.storages.fastSyncStateStorage,
        BlockValidator.validateHeaderAndBody),
      "fast-sync-controller")

    fastSyncController ! SyncController.StartSync

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        storagesInstance.dataSources.closeAll
      }
    })*/

  }
}
