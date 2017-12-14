package io.iohk.ethereum

import io.iohk.ethereum.dbfix.NodeTraversal
//import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
//import io.iohk.ethereum.network.discovery.DiscoveryListener
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.Logger

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object DbFix {
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
//
//      peerManager ! PeerManagerActor.StartConnecting
//      server ! ServerActor.StartServer(networkConfig.Server.listenAddress)
//
//      if (discoveryConfig.discoveryEnabled) {
//        discoveryListener ! DiscoveryListener.Start
//      }
//
//      peerDiscoveryManager // unlazy

      new NodeTraversal(blockchain)
    }
  }
}
