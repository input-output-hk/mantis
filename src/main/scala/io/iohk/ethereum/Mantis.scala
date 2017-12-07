package io.iohk.ethereum

import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.mining.Miner
import io.iohk.ethereum.network.discovery.DiscoveryListener
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.Logger

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object Mantis {

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

      if (discoveryConfig.discoveryEnabled) {
        discoveryListener ! DiscoveryListener.Start
      }

      syncController ! SyncController.Start

      if (miningConfig.miningEnabled) {
        miner ! Miner.StartMining
      }

      peerDiscoveryManager // unlazy

      maybeJsonRpcServer match {
        case Right(jsonRpcServer) if jsonRpcServerConfig.enabled => jsonRpcServer.run()
        case Left(error) if jsonRpcServerConfig.enabled => log.error(error)
        case _=> //Nothing
      }
    }

  }
}
