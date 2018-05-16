package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.utils.{KeyStoreConfig, Logger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Faucet extends Logger {

  def main(args: Array[String]): Unit = {
    val config = FaucetConfig(ConfigFactory.load())

    implicit val system = ActorSystem("Faucet-system")
    implicit val materializer = ActorMaterializer()

    val keyStore = new KeyStoreImpl(KeyStoreConfig.customKeyStoreConfig(config.keyStoreDir), new SecureRandom())
    val rpcClient = new RpcClient(config.rpcAddress)
    val api = new FaucetApi(rpcClient, keyStore, config)

    val bindingResultF = Http().bindAndHandle(api.route, config.listenInterface, config.listenPort)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"Faucet HTTP server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start faucet HTTP server", ex)
    }
  }

}
