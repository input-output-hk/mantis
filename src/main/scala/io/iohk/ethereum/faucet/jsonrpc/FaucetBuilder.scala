package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.ActorSystem
import io.iohk.ethereum.faucet.{FaucetConfigBuilder, FaucetSupervisor}
import io.iohk.ethereum.jsonrpc.server.controllers.ApisBase
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.security.{SSLContextBuilder, SecureRandomBuilder}
import io.iohk.ethereum.utils.{KeyStoreConfig, Logger}

import scala.concurrent.Await

trait ActorSystemBuilder {
  def systemName: String
  implicit lazy val system: ActorSystem = ActorSystem(systemName)
}

trait FaucetControllerBuilder {
  self: FaucetConfigBuilder with ActorSystemBuilder =>

  implicit val ec = system.dispatcher
}

trait FaucetRpcServiceBuilder {
  self: FaucetConfigBuilder
    with FaucetControllerBuilder
    with ActorSystemBuilder
    with SecureRandomBuilder
    with ShutdownHookBuilder
    with SSLContextBuilder =>

  val keyStore =
    new KeyStoreImpl(
      KeyStoreConfig.customKeyStoreConfig(faucetConfig.keyStoreDir),
      secureRandom
    )

  val walletRpcClient: WalletRpcClient =
    new WalletRpcClient(
      faucetConfig.rpcClient.address,
      faucetConfig.rpcClient.timeout,
      () => sslContext("faucet.rpc-client")
    )
  val walletService = new WalletService(walletRpcClient, keyStore, faucetConfig)
  val faucetSupervisor: FaucetSupervisor = new FaucetSupervisor(walletService, faucetConfig, shutdown)(system)
  val faucetRpcService = new FaucetRpcService(faucetConfig)
}

trait FaucetJsonRpcHealthCheckBuilder {
  self: FaucetRpcServiceBuilder =>

  val faucetJsonRpcHealthCheck = new FaucetJsonRpcHealthCheck(faucetRpcService)
}

trait ApisBuilder extends ApisBase {
  object Apis {
    val Faucet = "faucet"
  }

  override def available: List[String] = List(Apis.Faucet)
}

trait JsonRpcConfigBuilder {
  self: FaucetConfigBuilder with ApisBuilder =>

  lazy val availableApis: List[String] = available
  lazy val jsonRpcConfig: JsonRpcConfig = JsonRpcConfig(rawMantisConfig, availableApis)
  lazy val api = Apis
}

trait FaucetJsonRpcControllerBuilder {
  self: JsonRpcConfigBuilder with FaucetRpcServiceBuilder =>

  val faucetJsonRpcController = new FaucetJsonRpcController(faucetRpcService, jsonRpcConfig)
}

trait FaucetJsonRpcHttpServerBuilder {
  self: ActorSystemBuilder
    with JsonRpcConfigBuilder
    with SecureRandomBuilder
    with FaucetJsonRpcHealthCheckBuilder
    with FaucetJsonRpcControllerBuilder
    with SSLContextBuilder =>

  val faucetJsonRpcHttpServer = JsonRpcHttpServer(
    faucetJsonRpcController,
    faucetJsonRpcHealthCheck,
    jsonRpcConfig.httpServerConfig,
    secureRandom,
    () => sslContext("mantis.network.rpc.http")
  )
}

trait ShutdownHookBuilder {
  self: ActorSystemBuilder with FaucetConfigBuilder with Logger =>

  def shutdown(): Unit = {
    Await.ready(
      system
        .terminate()
        .map(
          _ ->
            log.info("actor system finished")
        )(system.dispatcher),
      faucetConfig.shutdownTimeout
    )
  }
}

class FaucetServer
    extends ActorSystemBuilder
    with FaucetConfigBuilder
    with ApisBuilder
    with JsonRpcConfigBuilder
    with SecureRandomBuilder
    with SSLContextBuilder
    with FaucetControllerBuilder
    with FaucetRpcServiceBuilder
    with FaucetJsonRpcHealthCheckBuilder
    with FaucetJsonRpcControllerBuilder
    with FaucetJsonRpcHttpServerBuilder
    with ShutdownHookBuilder
    with Logger {

  override def systemName: String = "Faucet-system"

  def start(): Unit = {
    log.info("About to start Faucet JSON-RPC server")
    startJsonRpcHttpServer()
  }

  private[this] def startJsonRpcHttpServer() =
    faucetJsonRpcHttpServer match {
      case Right(jsonRpcServer) => jsonRpcServer.run()
      case Left(error) => throw new RuntimeException(s"$error")
    }
}
