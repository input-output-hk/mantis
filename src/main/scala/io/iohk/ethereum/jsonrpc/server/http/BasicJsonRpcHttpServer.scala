package io.iohk.ethereum.jsonrpc.server.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.stream.ActorMaterializer
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class BasicJsonRpcHttpServer(val jsonRpcController: JsonRpcController, config: JsonRpcHttpServerConfig)
                            (implicit val actorSystem: ActorSystem)
  extends JsonRpcHttpServer with Logger {

  def run(): Unit = {
    implicit val materializer = ActorMaterializer()

    val bindingResultF = Http(actorSystem).bindAndHandle(route, config.interface, config.port)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"JSON RPC HTTP server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start JSON HTTP RPC server", ex)
    }
  }

  override def corsAllowedOrigins: HttpOriginRange = config.corsAllowedOrigins
}
