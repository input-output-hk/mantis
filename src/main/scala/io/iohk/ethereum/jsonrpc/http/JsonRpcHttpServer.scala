package io.iohk.ethereum.jsonrpc.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler, Route}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcErrors, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger
import org.json4s.JsonAST.JInt
import org.json4s.{DefaultFormats, native}

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

class JsonRpcHttpServer(jsonRpcController: JsonRpcController, config: JsonRpcHttpServerConfig)
                       (implicit val actorSystem: ActorSystem)
  extends Json4sSupport with Logger {

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats

  implicit def myRejectionHandler: RejectionHandler =
    RejectionHandler.newBuilder()
      .handle { case _: MalformedRequestContentRejection =>
        complete((StatusCodes.BadRequest, JsonRpcResponse("2.0", None, Some(JsonRpcErrors.ParseError), JInt(0))))
      }
      .result()

  val route: Route =
    (pathEndOrSingleSlash & post & entity(as[JsonRpcRequest])) { request =>
      handleRequest(request)
    }

  def run(): Unit = {
    implicit val materializer = ActorMaterializer()

    val bindingResultF = Http(actorSystem).bindAndHandle(route, config.interface, config.port)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"JSON RPC server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start JSON RPC server", ex)
    }
  }

  private def handleRequest(request: JsonRpcRequest) = {
    complete(jsonRpcController.handleRequest(request))
  }

}

object JsonRpcHttpServer {

  trait JsonRpcHttpServerConfig {
    val enabled: Boolean
    val interface: String
    val port: Int
  }

}
