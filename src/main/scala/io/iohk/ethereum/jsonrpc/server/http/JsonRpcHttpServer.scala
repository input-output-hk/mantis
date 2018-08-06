package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import ch.megard.akka.http.cors.javadsl.CorsRejection
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.buildinfo.MantisBuildInfo
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.utils.{ConfigUtils, JsonUtils, Logger}
import org.json4s.JsonAST.JInt
import org.json4s.{DefaultFormats, native}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

trait JsonRpcHttpServer extends Json4sSupport {
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
    handleRejections(myRejectionHandler) {
      (path("healthcheck") & pathEndOrSingleSlash & get) {
        handleHealthcheck()
      } ~
        (path("buildinfo") & pathEndOrSingleSlash & get) {
          handleBuildInfo()
        } ~
        (pathEndOrSingleSlash & post) {
          entity(as[JsonRpcRequest]) { request =>
            handleRequest(request)
          } ~ entity(as[Seq[JsonRpcRequest]]) { request =>
            handleBatchRequest(request)
          }
        }
    }
  }

  /**
    * Try to start JSON RPC server
    */
  def run(): Unit

  private[this] final val buildInfoResponse: HttpResponse = {
    val json = JsonUtils.pretty(MantisBuildInfo.toMap)

    HttpResponse(
      status = StatusCodes.OK,
      entity = HttpEntity(ContentTypes.`application/json`, json)
    )
  }

  private[this] final val buildInfoRoute: StandardRoute = complete(buildInfoResponse)

  private[this] def handleBuildInfo() = buildInfoRoute

  private[this] def handleHealthcheck() = {
    val responseF = jsonRpcController.healthcheck()

    val httpResponseF =
      responseF.map {
        case response if response.isOK ⇒
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )

        case response ⇒
          HttpResponse(
            status = StatusCodes.InternalServerError,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )
      }

    complete(httpResponseF)
  }

  private def handleRequest(request: JsonRpcRequest) = {
    complete(jsonRpcController.handleRequest(request))
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]) = {
    complete(Future.sequence(requests.map(request => jsonRpcController.handleRequest(request))))
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
    val corsAllowedOrigins: HttpOriginRange
  }

  object JsonRpcHttpServerConfig {
    import com.typesafe.config.{Config ⇒ TypesafeConfig}

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
