package io.iohk.ethereum.nodebuilder

import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.network.discovery.DiscoveryListener

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

/**
 * A standard node is everything Ethereum prescribes except the consensus algorithm,
 * which is plugged in dynamically.
 *
 * The design is historically related to the initial cake-pattern-based
 * [[io.iohk.ethereum.nodebuilder.Node Node]].
 *
 * @see [[io.iohk.ethereum.nodebuilder.Node Node]]
 */
class StdNode extends Node {
  private[this] def loadGenesisData(): Unit = genesisDataLoader.loadGenesisData()

  private[this] def startPeerManager(): Unit = peerManager ! PeerManagerActor.StartConnecting

  private[this] def startServer(): Unit = server ! ServerActor.StartServer(networkConfig.Server.listenAddress)

  private[this] def startDiscoveryListener(): Unit =
    if (discoveryConfig.discoveryEnabled) {
      discoveryListener ! DiscoveryListener.Start
    }

  private[this] def startSyncController(): Unit = syncController ! SyncController.Start

  private[this] def startConsensus(): Unit = consensus.startProtocol(this)

  private[this] def startDiscoveryManager(): Unit = peerDiscoveryManager // unlazy

  private[this] def startJsonRpcServer(): Unit =
    maybeJsonRpcServer match {
      case Right(jsonRpcServer) if jsonRpcServerConfig.enabled => jsonRpcServer.run()
      case Left(error) if jsonRpcServerConfig.enabled => log.error(error)
      case _=> //Nothing
    }

  def start(): Unit = {
    loadGenesisData()

    startPeerManager()

    startServer()

    startDiscoveryListener()

    startSyncController()

    startConsensus()

    startDiscoveryManager()

    startJsonRpcServer()
  }

  override def shutdown(): Unit = {
    def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
      case Failure(e) => log.warn("Error while shutting down...", e)
      case Success(_) =>
    }

    tryAndLogFailure(() => consensus.stopProtocol())
    tryAndLogFailure(() => Await.ready(actorSystem.terminate, shutdownTimeoutDuration))
    tryAndLogFailure(() => storagesInstance.dataSources.closeAll())
  }
}
