package io.iohk.ethereum.jsonrpc.server

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler, Route}
import ch.megard.akka.http.cors.javadsl.CorsRejection
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcErrors, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger
import org.json4s.JsonAST.JInt
import org.json4s.{DefaultFormats, native}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait JsonRpcServer extends Json4sSupport {
  val jsonRpcController: JsonRpcController

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats

  def corsAllowedOrigins: HttpOriginRange

  val corsSettings = CorsSettings.defaultSettings.copy(
    allowGenericHttpRequests = true,
    allowedOrigins = corsAllowedOrigins
  )

  implicit def myRejectionHandler: RejectionHandler =
    RejectionHandler.newBuilder()
      .handle {
        case _: MalformedRequestContentRejection =>
          complete((StatusCodes.BadRequest, JsonRpcResponse("2.0", None, Some(JsonRpcErrors.ParseError), JInt(0))))
        case _: CorsRejection =>
          complete(StatusCodes.Forbidden)
      }
      .result()

  val route: Route = cors(corsSettings) {
    (pathEndOrSingleSlash & post) {
      entity(as[JsonRpcRequest]) { request =>
        handleRequest(request)
      } ~ entity(as[Seq[JsonRpcRequest]]) { request =>
        handleBatchRequest(request)
      }
    }
  }

  /**
    * Try to start JSON RPC server
    */
  def run(): Unit

  private def handleRequest(request: JsonRpcRequest) = {
    complete(jsonRpcController.handleRequest(request))
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]) = {
    complete(Future.sequence(requests.map(request => jsonRpcController.handleRequest(request))))
  }
}

object JsonRpcServer extends Logger {

  def apply(jsonRpcController: JsonRpcController, config: JsonRpcServerConfig, secureRandom: SecureRandom)
           (implicit actorSystem: ActorSystem): Either[String, JsonRpcServer] = config.mode match {
    case "http" => Right(new JsonRpcHttpServer(jsonRpcController, config)(actorSystem))
    case "https" => Right(new JsonRpcHttpsServer(jsonRpcController, config, secureRandom)(actorSystem))
    case _ => Left(s"Cannot start JSON RPC server: Invalid mode ${config.mode} selected")
  }

  trait JsonRpcServerConfig {
    val mode: String
    val enabled: Boolean
    val interface: String
    val port: Int
    val certificateKeyStorePath: Option[String]
    val certificateKeyStoreType: Option[String]
    val certificatePasswordFile: Option[String]
    val corsAllowedOrigins: HttpOriginRange
  }


}
