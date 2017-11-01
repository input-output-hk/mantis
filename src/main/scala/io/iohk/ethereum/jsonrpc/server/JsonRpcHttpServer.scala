package io.iohk.ethereum.jsonrpc.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.stream.ActorMaterializer
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.JsonRpcServer.JsonRpcServerConfig
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class JsonRpcHttpServer(val jsonRpcController: JsonRpcController, config: JsonRpcServerConfig)
                       (implicit val actorSystem: ActorSystem)
  extends JsonRpcServer with Logger {

  def run(): Unit = {
    implicit val materializer = ActorMaterializer()

    val bindingResultF = Http(actorSystem).bindAndHandle(route, config.interface, config.port)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"JSON RPC HTTP server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start JSON HTTP RPC server", ex)
    }
  }

  override def allowedOrigins: Option[HttpOriginRange] = config.corsAllowedOrigins
}
