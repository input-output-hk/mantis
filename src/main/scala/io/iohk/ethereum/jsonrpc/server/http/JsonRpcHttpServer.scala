package io.iohk.ethereum.jsonrpc.server.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler, Route, StandardRoute}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcErrors, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.{ConfigUtils, Logger}
import java.security.SecureRandom

import ch.megard.akka.http.cors.scaladsl.CorsRejection
import org.json4s.JsonAST.JInt
import org.json4s.{DefaultFormats, native}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

trait JsonRpcHttpServer extends Json4sSupport {
  val jsonRpcController: JsonRpcController

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats

  def corsAllowedOrigins: HttpOriginMatcher

  val corsSettings = CorsSettings.defaultSettings
    .withAllowGenericHttpRequests(true)
    .withAllowedOrigins(corsAllowedOrigins)

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

  private def handleRequest(request: JsonRpcRequest): StandardRoute = {
    val resp: Future[JsonRpcResponse] = jsonRpcController.handleRequest(request)
    complete(resp)
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]): StandardRoute = {
    val value: Seq[Future[JsonRpcResponse]] = requests.map(request => jsonRpcController.handleRequest(request))
    val fut: Future[Seq[JsonRpcResponse]] = Future.sequence(value)
    complete(fut)
  }
}

object JsonRpcHttpServer extends Logger {

  def apply(jsonRpcController: JsonRpcController, config: JsonRpcHttpServerConfig, secureRandom: SecureRandom)
           (implicit actorSystem: ActorSystem): Either[String, JsonRpcHttpServer] = config.mode match {
    case "http" => Right(new BasicJsonRpcHttpServer(jsonRpcController, config)(actorSystem))
    case "https" => Right(new JsonRpcHttpsServer(jsonRpcController, config, secureRandom)(actorSystem))
    case _ => Left(s"Cannot start JSON RPC server: Invalid mode ${config.mode} selected")
  }

  trait JsonRpcHttpServerConfig {
    val mode: String
    val enabled: Boolean
    val interface: String
    val port: Int
    val certificateKeyStorePath: Option[String]
    val certificateKeyStoreType: Option[String]
    val certificatePasswordFile: Option[String]
    val corsAllowedOrigins: HttpOriginMatcher
  }

  object JsonRpcHttpServerConfig {
    import com.typesafe.config.{Config => TypesafeConfig}

    def apply(mantisConfig: TypesafeConfig): JsonRpcHttpServerConfig = {
      val rpcHttpConfig = mantisConfig.getConfig("network.rpc.http")

      new JsonRpcHttpServerConfig {
        override val mode: String = rpcHttpConfig.getString("mode")
        override val enabled: Boolean = rpcHttpConfig.getBoolean("enabled")
        override val interface: String = rpcHttpConfig.getString("interface")
        override val port: Int = rpcHttpConfig.getInt("port")

        override val corsAllowedOrigins = ConfigUtils.parseCorsAllowedOrigins(rpcHttpConfig, "cors-allowed-origins")

        override val certificateKeyStorePath: Option[String] = Try(rpcHttpConfig.getString("certificate-keystore-path")).toOption
        override val certificateKeyStoreType: Option[String] = Try(rpcHttpConfig.getString("certificate-keystore-type")).toOption
        override val certificatePasswordFile: Option[String] = Try(rpcHttpConfig.getString("certificate-password-file")).toOption
      }
    }
  }
}
