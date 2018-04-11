package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.utils.{Config, Logger}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.{Failure, Success}

object Faucet extends Logger {

  def main(args: Array[String]): Unit = {
    val config = FaucetConfig(Config.config)

    val rpcClient = new RpcClient(config.rpcAddress)
    val keyStore = new KeyStoreImpl(config.keyStoreDir, new SecureRandom())
    val api = new FaucetApi(rpcClient, keyStore, config)

    implicit val system = ActorSystem("Faucet-system")
    implicit val materializer = ActorMaterializer()

    val bindingResultF = Http().bindAndHandle(api.route, config.listenInterface, config.listenPort)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"Faucet HTTP server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start faucet HTTP server", ex)
    }
  }

}
