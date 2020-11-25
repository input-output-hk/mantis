package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom
import java.time.Clock

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import ch.megard.akka.http.cors.javadsl.CorsRejection
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.twitter.util.LruMap
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.faucet.jsonrpc.FaucetJsonRpcController
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.utils.{ConfigUtils, Logger}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.json4s.{DefaultFormats, JInt, native}

import scala.concurrent.duration.{FiniteDuration, _}

trait JsonRpcHttpServer extends Json4sSupport {
  val jsonRpcController: JsonRpcBaseController
  val jsonRpcHealthChecker: JsonRpcHealthChecker

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats + JsonSerializers.RpcErrorJsonSerializer

  def corsAllowedOrigins: HttpOriginMatcher

  val minRequestInterval: FiniteDuration

  val latestTimestampCacheSize: Int

  val latestRequestTimestamps: LruMap[RemoteAddress, Long]

  val ipTrackingEnabled: Boolean

  val clock: Clock = Clock.systemUTC()

  val corsSettings = CorsSettings.defaultSettings
    .withAllowGenericHttpRequests(true)
    .withAllowedOrigins(corsAllowedOrigins)

  implicit def myRejectionHandler: RejectionHandler =
    RejectionHandler
      .newBuilder()
      .handle {
        case _: MalformedRequestContentRejection =>
          complete((StatusCodes.BadRequest, JsonRpcResponse("2.0", None, Some(JsonRpcError.ParseError), JInt(0))))
        case _: CorsRejection =>
          complete(StatusCodes.Forbidden)
      }
      .result()

  val route: Route = cors(corsSettings) {
    (path("healthcheck") & pathEndOrSingleSlash & get) {
      handleHealthcheck()
    } ~ (pathEndOrSingleSlash & post) {
      (extractClientIP & entity(as[JsonRpcRequest])){
        (clientAddress, request) => handleRequest(clientAddress, request)
      } ~ entity(as[Seq[JsonRpcRequest]]) { request =>
        handleBatchRequest(request)
      }
    }
  }

  def handleRequest(clientAddress: RemoteAddress, request: JsonRpcRequest): StandardRoute = {
    if (ipTrackingEnabled && request.method != FaucetJsonRpcController.Status){
      handleRestrictedRequest(clientAddress, request)
    } else complete(jsonRpcController.handleRequest(request).runToFuture)
  }

  def handleRestrictedRequest (clientAddress: RemoteAddress, request: JsonRpcRequest): StandardRoute = {
    val timeMillis = clock.instant().toEpochMilli
    val latestRequestTimestamp = latestRequestTimestamps.getOrElse(clientAddress, 0L)

    if (latestRequestTimestamp + minRequestInterval.toMillis < timeMillis) {
      latestRequestTimestamps.put(clientAddress, timeMillis)
      complete(jsonRpcController.handleRequest(request).runToFuture)
    } else complete(StatusCodes.TooManyRequests)
  }

  /**
    * Try to start JSON RPC server
    */
  def run(): Unit

  private def handleHealthcheck(): StandardRoute = {
    val responseF = jsonRpcHealthChecker.healthCheck()
    val httpResponseF =
      responseF.map {
        case response if response.isOK =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )
        case response =>
          HttpResponse(
            status = StatusCodes.InternalServerError,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )
      }
    complete(httpResponseF)
  }

  private def handleRequest(request: JsonRpcRequest) = {
    complete(jsonRpcController.handleRequest(request).runToFuture)
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]) = {
    if (!ipTrackingEnabled) {
      complete {
        Task
          .traverse(requests)(request => jsonRpcController.handleRequest(request))
          .runToFuture
      }
    } else complete(StatusCodes.MethodNotAllowed)
  }
}

object JsonRpcHttpServer extends Logger {

  def apply(
             jsonRpcController: JsonRpcBaseController,
             jsonRpcHealthchecker: JsonRpcHealthChecker,
             config: JsonRpcHttpServerConfig,
             secureRandom: SecureRandom
           )(implicit actorSystem: ActorSystem): Either[String, JsonRpcHttpServer] =
    config.mode match {
      case "http" => Right(new InsecureJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config)(actorSystem))
      case "https" =>
        Right(new SecureJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config, secureRandom)(actorSystem))
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
    val ipTrackingEnabled: Boolean
    val minRequestInterval: FiniteDuration
    val latestTimestampCacheSize: Int
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

        override val certificateKeyStorePath: Option[String] =
          ConfigUtils.getOptionalValue(rpcHttpConfig, _.getString, "certificate-keystore-path")
        override val certificateKeyStoreType: Option[String] =
          ConfigUtils.getOptionalValue(rpcHttpConfig, _.getString, "certificate-keystore-type")
        override val certificatePasswordFile: Option[String] =
          ConfigUtils.getOptionalValue(rpcHttpConfig, _.getString, "certificate-password-file")

        private val ipTrackingConfig = rpcHttpConfig.getConfig("ip-restriction")

        override val ipTrackingEnabled: Boolean = ipTrackingConfig.getBoolean("enabled")
        override val minRequestInterval: FiniteDuration = ipTrackingConfig.getDuration("min-request-interval").toMillis.millis
        override val latestTimestampCacheSize: Int = ipTrackingConfig.getInt("latest-timestamp-cache-size")
      }
    }
  }
}
