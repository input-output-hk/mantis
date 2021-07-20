package io.iohk.ethereum.nodebuilder

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.mining.StdMiningBuilder
import io.iohk.ethereum.metrics.Metrics
import io.iohk.ethereum.metrics.MetricsConfig
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.ServerActor
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager
import io.iohk.ethereum.utils.Config

/** A standard node is everything Ethereum prescribes except the mining algorithm,
  * which is plugged in dynamically.
  *
  * The design is historically related to the initial cake-pattern-based
  * [[io.iohk.ethereum.nodebuilder.Node Node]].
  *
  * @see [[io.iohk.ethereum.nodebuilder.Node Node]]
  */
abstract class BaseNode extends Node {
  def start(): Unit = {
    startMetricsClient()

    loadGenesisData()

    startPeerManager()

    startPortForwarding()

    startServer()

    startSyncController()

    startMining()

    startDiscoveryManager()

    startJsonRpcHttpServer()

    startJsonRpcIpcServer()
  }

  private[this] def startMetricsClient(): Unit = {
    val metricsConfig = MetricsConfig(Config.config)
    Metrics.configure(metricsConfig) match {
      case Success(_) =>
        log.info("Metrics started")
      case Failure(exception) => throw exception
    }
  }

  private[this] def loadGenesisData(): Unit =
    if (!Config.testmode) genesisDataLoader.loadGenesisData()

  private[this] def startPeerManager(): Unit = peerManager ! PeerManagerActor.StartConnecting

  private[this] def startServer(): Unit = server ! ServerActor.StartServer(networkConfig.Server.listenAddress)

  private[this] def startSyncController(): Unit = syncController ! SyncProtocol.Start

  private[this] def startMining(): Unit = mining.startProtocol(this)

  private[this] def startDiscoveryManager(): Unit = peerDiscoveryManager ! PeerDiscoveryManager.Start

  private[this] def startJsonRpcHttpServer(): Unit =
    maybeJsonRpcHttpServer match {
      case Right(jsonRpcServer) if jsonRpcConfig.httpServerConfig.enabled => jsonRpcServer.run()
      case Left(error) if jsonRpcConfig.httpServerConfig.enabled          => log.error(error)
      case _                                                              => //Nothing
    }

  private[this] def startJsonRpcIpcServer(): Unit =
    if (jsonRpcConfig.ipcServerConfig.enabled) jsonRpcIpcServer.run()

  override def shutdown(): Unit = {
    def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
      case Failure(e) => log.warn("Error while shutting down...", e)
      case Success(_) =>
    }

    tryAndLogFailure(() => peerDiscoveryManager ! PeerDiscoveryManager.Stop)
    tryAndLogFailure(() => mining.stopProtocol())
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
    tryAndLogFailure(() => Await.ready(stopPortForwarding(), shutdownTimeoutDuration))
    if (jsonRpcConfig.ipcServerConfig.enabled) {
      tryAndLogFailure(() => jsonRpcIpcServer.close())
    }
    tryAndLogFailure(() => Metrics.get().close())
    tryAndLogFailure(() => storagesInstance.dataSource.close())
  }
}

class StdNode extends BaseNode with StdMiningBuilder
