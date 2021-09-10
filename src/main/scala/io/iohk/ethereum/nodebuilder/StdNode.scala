package io.iohk.ethereum.nodebuilder

import akka.actor.typed.ActorSystem
import akka.util.ByteString

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
import io.iohk.ethereum.nodebuilder.tooling.PeriodicConsistencyCheck
import io.iohk.ethereum.nodebuilder.tooling.StorageConsistencyChecker
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Hex

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

    fixDatabase()

    loadGenesisData()

    runDBConsistencyCheck()

    startPeerManager()

    startPortForwarding()

    startServer()

    startSyncController()

    startMining()

    startDiscoveryManager()

    startJsonRpcHttpServer()

    startJsonRpcIpcServer()

    startPeriodicDBConsistencyCheck()
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

  private[this] def runDBConsistencyCheck(): Unit =
    StorageConsistencyChecker.checkStorageConsistency(
      storagesInstance.storages.appStateStorage.getBestBlockNumber(),
      storagesInstance.storages.blockNumberMappingStorage,
      storagesInstance.storages.blockHeadersStorage,
      shutdown
    )(log)

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

  def startPeriodicDBConsistencyCheck(): Unit =
    if (Config.Db.periodicConsistencyCheck)
      ActorSystem(
        PeriodicConsistencyCheck.start(
          storagesInstance.storages.appStateStorage,
          storagesInstance.storages.blockNumberMappingStorage,
          storagesInstance.storages.blockHeadersStorage,
          shutdown
        ),
        "PeriodicDBConsistencyCheck"
      )

  override def shutdown: () => Unit = () => {
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

  def fixDatabase(): Unit = {
    // FIXME this is a temporary solution to avoid an incompatibility due to the introduction of the best block hash
    // We can remove this fix when we release an incompatible version.
    val bestBlockInfo = storagesInstance.storages.appStateStorage.getBestBlockInfo()
    if (bestBlockInfo.hash == ByteString.empty && bestBlockInfo.number > 0) {
      log.warn("Fixing best block hash into database for block {}", bestBlockInfo.number)
      storagesInstance.storages.blockNumberMappingStorage.get(bestBlockInfo.number) match {
        case Some(hash) =>
          log.warn("Putting {} as the best block hash", Hex.toHexString(hash.toArray))
          storagesInstance.storages.appStateStorage.putBestBlockInfo(bestBlockInfo.copy(hash = hash)).commit()
        case None =>
          log.error("No block found for number {} when trying to fix database", bestBlockInfo.number)
      }

    }

  }
}

class StdNode extends BaseNode with StdMiningBuilder
