package io.iohk.ethereum.nodebuilder

import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.StdConsensusBuilder
import io.iohk.ethereum.metrics.{Metrics, MetricsConfig}
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.testmode.{TestLedgerBuilder, TestmodeConsensusBuilder}
import io.iohk.ethereum.utils.Config
import scala.concurrent.ExecutionContext.Implicits.global
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

  private[this] def startSyncController(): Unit = syncController ! SyncProtocol.Start

  private[this] def startConsensus(): Unit = consensus.startProtocol(this)

  private[this] def startDiscoveryManager(): Unit = peerDiscoveryManager ! PeerDiscoveryManager.Start

  private[this] def startJsonRpcHttpServer(): Unit =
    maybeJsonRpcHttpServer match {
      case Right(jsonRpcServer) if jsonRpcConfig.httpServerConfig.enabled => jsonRpcServer.run()
      case Left(error) if jsonRpcConfig.httpServerConfig.enabled => log.error(error)
      case _ => //Nothing
    }

  private[this] def startJsonRpcIpcServer(): Unit = {
    if (jsonRpcConfig.ipcServerConfig.enabled) jsonRpcIpcServer.run()
  }

  private[this] def startMetricsClient(): Unit = {
    val metricsConfig = MetricsConfig(Config.config)
    Metrics.configure(metricsConfig) match {
      case Success(_) =>
        log.info("Metrics started")
      case Failure(exception) => throw exception
    }
  }

  def start(): Unit = {
    startMetricsClient()

    loadGenesisData()

    startPeerManager()

    startServer()

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

    tryAndLogFailure(() => peerDiscoveryManager ! PeerDiscoveryManager.Stop)
    tryAndLogFailure(() => consensus.stopProtocol())
    tryAndLogFailure(() =>
      Await.ready(
        system
          .terminate()
          .map(
            _ ->
              log.info("actor system finished")
          ),
        shutdownTimeoutDuration
      )
    )
    if (jsonRpcConfig.ipcServerConfig.enabled) {
      tryAndLogFailure(() => jsonRpcIpcServer.close())
    }
    tryAndLogFailure(() => Metrics.get().close())
    tryAndLogFailure(() => storagesInstance.dataSource.close())
  }
}

class StdNode extends BaseNode with StdLedgerBuilder with StdConsensusBuilder
class TestNode extends BaseNode with TestLedgerBuilder with TestmodeConsensusBuilder with TestServiceBuilder
