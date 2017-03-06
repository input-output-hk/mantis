package io.iohk.ethereum

import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.network.ServerActor
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.Logger

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

object App {

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
      fastSyncController ! FastSyncController.StartFastSync

      if(rpcServerConfig.enabled) startJSONRpcServer()
    }

  }
}
