package io.iohk.ethereum.faucet.jsonrpc

import java.security.SecureRandom

import akka.actor.ActorSystem
import io.iohk.ethereum.faucet.FaucetConfigBuilder
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.{KeyStoreConfig, Logger}

import scala.concurrent.Future


trait ActorSystemBuilder {
  def systemName: String
  implicit lazy val system: ActorSystem = ActorSystem(systemName)
}

trait FaucetControllerBuilder {
  self: FaucetConfigBuilder with ActorSystemBuilder  =>

  implicit val ec = system.dispatcher

  /*private val walletRpcClient =
    new WalletRpcClient(faucetConfig.walletRpcAddress, sslContext.toOption)

  private val faucetService: FaucetService = new FaucetServiceImpl(walletRpcClient)
*/

}

trait FaucetRpcServiceBuilder {
  self: FaucetConfigBuilder with FaucetControllerBuilder with ActorSystemBuilder =>

  val keyStore = new KeyStoreImpl(KeyStoreConfig.customKeyStoreConfig(faucetConfig.keyStoreDir), new SecureRandom())
  val rpcClient = new RpcClient(faucetConfig.rpcAddress)
  val faucetRpcService = new FaucetRpcService(rpcClient, keyStore, faucetConfig)
}

trait FaucetJsonRpcHealthCheckBuilder {
  self: FaucetRpcServiceBuilder =>

  val faucetJsonRpcHealthCheck = new FaucetJsonRpcHealthCheck(faucetRpcService)
}

trait JsonRpcConfigBuilder { self: FaucetConfigBuilder =>

  object Apis {
    val FAUCET = "faucet"

    val available = Seq(FAUCET)
  }

  val availableApis: List[String] = Apis.available.toList //TODO: mmmmm
  lazy val jsonRpcConfig: JsonRpcConfig = JsonRpcConfig(rawMantisConfig, Apis.available.toList)
}

trait FaucetJsonRpcControllerBuilder {
  self: JsonRpcConfigBuilder with FaucetRpcServiceBuilder =>

  val faucetJsonRpcController = new FaucetJsonRpcController(
    new FaucetJRC(faucetRpcService, jsonRpcConfig).handleFaucetRequest,
    jsonRpcConfig
  )
}

trait FaucetJsonRpcHttpServerBuilder {
  self: ActorSystemBuilder
    with JsonRpcConfigBuilder
    with SecureRandomBuilder
    with FaucetJsonRpcHealthCheckBuilder
    with FaucetJsonRpcControllerBuilder =>

  val faucetJsonRpcHttpServer = JsonRpcHttpServer(
    faucetJsonRpcController,
    faucetJsonRpcHealthCheck,
    jsonRpcConfig.httpServerConfig,
    secureRandom,
   // DispatcherId("midnight.async.dispatchers.json-rpc-http"), //TODO: add dispatcher
    //() => sslContext
  )
}

class FaucetServer extends
  ActorSystemBuilder
  with FaucetConfigBuilder
  with JsonRpcConfigBuilder
  with SecureRandomBuilder
  with FaucetControllerBuilder
  with FaucetRpcServiceBuilder
  with FaucetJsonRpcHealthCheckBuilder
  with FaucetJsonRpcControllerBuilder
  with FaucetJsonRpcHttpServerBuilder
  with Logger {

  override def systemName: String = "Faucet-system"

  def start(): Unit = {
    log.info("About to start Faucet server")

    log.info("About to start Faucet JSON-RPC server")
    startJsonRpcHttpServer()
  }

  //TODO: load if jsonRpcConfig.httpServerConfig.enabled
  private[this] def startJsonRpcHttpServer() =
    faucetJsonRpcHttpServer match {
      case Right(jsonRpcServer) => jsonRpcServer.run()
      case Left(error) =>
        Future.failed(new RuntimeException(s"$error"))
    }
}
