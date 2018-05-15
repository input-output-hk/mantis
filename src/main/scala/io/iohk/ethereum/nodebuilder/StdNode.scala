package io.iohk.ethereum.nodebuilder

import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.consensus.StdConsensusBuilder
import io.iohk.ethereum.metrics.{Metrics, MetricsClient}
import io.iohk.ethereum.network.discovery.DiscoveryListener
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.testmode.{TestLedgerBuilder, TestmodeConsensusBuilder}
import io.iohk.ethereum.utils.Config

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
abstract class BaseNode extends Node {
  private[this] def loadGenesisData(): Unit = {
    if (!Config.testmode) genesisDataLoader.loadGenesisData()
  }

  private[this] def startPeerManager(): Unit = peerManager ! PeerManagerActor.StartConnecting

  private[this] def startServer(): Unit = server ! ServerActor.StartServer(networkConfig.Server.listenAddress)

  private[this] def startDiscoveryListener(): Unit =
    if (discoveryConfig.discoveryEnabled) {
      discoveryListener ! DiscoveryListener.Start
    }

  private[this] def startSyncController(): Unit = syncController ! SyncController.Start

  private[this] def startConsensus(): Unit = consensus.startProtocol(this)

  private[this] def startDiscoveryManager(): Unit = peerDiscoveryManager // unlazy

  private[this] def startJsonRpcHttpServer(): Unit =
    maybeJsonRpcHttpServer match {
      case Right(jsonRpcServer) if jsonRpcConfig.httpServerConfig.enabled => jsonRpcServer.run()
      case Left(error) if jsonRpcConfig.httpServerConfig.enabled => log.error(error)
      case _=> //Nothing
    }

  private[this] def startJsonRpcIpcServer(): Unit = {
    if (jsonRpcConfig.ipcServerConfig.enabled) jsonRpcIpcServer.run()
  }

  private[this] def startMetricsClient(): Unit = {
    MetricsClient.configure(Config.config)

    // Just produce a point in the graphs to signify Mantis has been (re)started.
    MetricsClient.get().gauge(Metrics.StartEvent, 1L)
  }

  def start(): Unit = {
    startMetricsClient()

    loadGenesisData()

    startPeerManager()

    startServer()

    startDiscoveryListener()

    startSyncController()

    startConsensus()

    startDiscoveryManager()

    startJsonRpcHttpServer()
    startJsonRpcIpcServer()
  }

  override def shutdown(): Unit = {
    def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
      case Failure(e) => log.warn("Error while shutting down...", e)
      case Success(_) =>
    }

    tryAndLogFailure(() => consensus.stopProtocol())
    tryAndLogFailure(() => Await.ready(system.terminate, shutdownTimeoutDuration))
    tryAndLogFailure(() => storagesInstance.dataSources.closeAll())
    if (jsonRpcConfig.ipcServerConfig.enabled) {
      tryAndLogFailure(() => jsonRpcIpcServer.close())
    }
    tryAndLogFailure(() => MetricsClient.get().gauge(Metrics.StopEvent, 1L))
    tryAndLogFailure(() => MetricsClient.get().close())
  }
}

class StdNode extends BaseNode with StdLedgerBuilder with StdConsensusBuilder
class TestNode extends BaseNode with TestLedgerBuilder with TestmodeConsensusBuilder with TestServiceBuilder
