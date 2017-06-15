package io.iohk.ethereum

import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.nodebuilder.Node

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object App {

  def main(args: Array[String]): Unit = {

    new Node with Logger {

      def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
        case Failure(e) => log.warn("Error while shutting down...", e)
        case Success(_) =>
      }

      override def shutdown(): Unit = {
        tryAndLogFailure(() => Await.ready(actorSystem.terminate, shutdownTimeoutDuration))
        tryAndLogFailure(() => storagesInstance.dataSources.closeAll())
      }

      genesisDataLoader.loadGenesisData()

      peerManager ! PeerManagerActor.StartConnecting
      server ! ServerActor.StartServer(networkConfig.Server.listenAddress)
      syncController ! SyncController.StartSync

      if (jsonRpcHttpServerConfig.enabled) jsonRpcHttpServer.run()
    }

  }
}
