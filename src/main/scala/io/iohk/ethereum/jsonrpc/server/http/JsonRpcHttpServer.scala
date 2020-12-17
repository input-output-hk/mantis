package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom
import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import ch.megard.akka.http.cors.javadsl.CorsRejection
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.{Config => TypesafeConfig}
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.faucet.jsonrpc.FaucetJsonRpcController
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.security.SSLError
import io.iohk.ethereum.utils.{BuildInfo, ConfigUtils, Logger}
import javax.net.ssl.SSLContext
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.json4s.native.Serialization
import org.json4s.{DefaultFormats, JInt, native}
import scala.concurrent.duration.{FiniteDuration, _}

trait JsonRpcHttpServer extends Json4sSupport with RateLimit with Logger {
  val jsonRpcController: JsonRpcBaseController
  val jsonRpcHealthChecker: JsonRpcHealthChecker
  val config: JsonRpcHttpServerConfig

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats + JsonSerializers.RpcErrorJsonSerializer

  def corsAllowedOrigins: HttpOriginMatcher

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
    } ~ (path("buildinfo") & pathEndOrSingleSlash & get) {
      handleBuildInfo()
    } ~ (pathEndOrSingleSlash & post) {
      (extractClientIP & entity(as[JsonRpcRequest])) { (clientAddress, request) =>
        handleRequest(clientAddress, request)
      } ~ entity(as[Seq[JsonRpcRequest]]) { request =>
        handleBatchRequest(request)
      }
    }
  }

  def handleRequest(clientAddress: RemoteAddress, request: JsonRpcRequest): StandardRoute = {
    //FIXME: FaucetJsonRpcController.Status should be part of a Healthcheck request or alike.
    // As a temporary solution, it is being excluded from the Rate Limit.
    if (config.rateLimit.enabled && request.method != FaucetJsonRpcController.Status) {
      handleRateLimitedRequest(clientAddress, request)
    } else complete(handleResponse(jsonRpcController.handleRequest(request)).runToFuture)
  }

  def handleRateLimitedRequest(clientAddress: RemoteAddress, request: JsonRpcRequest): StandardRoute = {
    if (isBelowRateLimit(clientAddress))
      complete(handleResponse(jsonRpcController.handleRequest(request)).runToFuture)
    else {
      log.warn(s"Request limit exceeded for ip ${clientAddress.toIP.getOrElse("unknown")}")
      complete(
        (StatusCodes.TooManyRequests, JsonRpcError.RateLimitError(config.rateLimit.minRequestInterval.toSeconds))
      )
    }
  }

  private def handleResponse(f: Task[JsonRpcResponse]): Task[(StatusCode, JsonRpcResponse)] = f map { jsonRpcResponse =>
    jsonRpcResponse.error match {
      case Some(JsonRpcError(error, _, _))
          if List(JsonRpcError.InvalidRequest.code, JsonRpcError.ParseError.code, JsonRpcError.InvalidParams().code)
            .contains(error) =>
        (StatusCodes.BadRequest, jsonRpcResponse)
      case _ => (StatusCodes.OK, jsonRpcResponse)
    }
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
    complete(httpResponseF.runToFuture)
  }

  private def handleBuildInfo(): StandardRoute = {
    val buildInfo = Serialization.writePretty(BuildInfo.toMap)(DefaultFormats)
    complete(
      HttpResponse(
        status = StatusCodes.OK,
        entity = HttpEntity(ContentTypes.`application/json`, buildInfo)
      )
    )
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]) = {
    if (!config.rateLimit.enabled) {
      complete {
        Task
          .traverse(requests)(request => jsonRpcController.handleRequest(request))
          .runToFuture
      }
    } else complete(StatusCodes.MethodNotAllowed, JsonRpcError.MethodNotFound)
  }
}

object JsonRpcHttpServer extends Logger {

  def apply(
      jsonRpcController: JsonRpcBaseController,
      jsonRpcHealthchecker: JsonRpcHealthChecker,
      config: JsonRpcHttpServerConfig,
      secureRandom: SecureRandom,
      fSslContext: () => Either[SSLError, SSLContext]
  )(implicit actorSystem: ActorSystem): Either[String, JsonRpcHttpServer] =
    config.mode match {
      case "http" => Right(new InsecureJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config)(actorSystem))
      case "https" =>
        Right(
          new SecureJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config, secureRandom, fSslContext)(
            actorSystem
          )
        )
      case _ => Left(s"Cannot start JSON RPC server: Invalid mode ${config.mode} selected")
    }

  trait RateLimitConfig {
    val enabled: Boolean
    val minRequestInterval: FiniteDuration
    val latestTimestampCacheSize: Int
  }

  object RateLimitConfig {
    def apply(rateLimitConfig: TypesafeConfig): RateLimitConfig =
      new RateLimitConfig {
        override val enabled: Boolean = rateLimitConfig.getBoolean("enabled")
        override val minRequestInterval: FiniteDuration =
          rateLimitConfig.getDuration("min-request-interval").toMillis.millis
        override val latestTimestampCacheSize: Int = rateLimitConfig.getInt("latest-timestamp-cache-size")
      }
  }

  trait JsonRpcHttpServerConfig {
    val mode: String
    val enabled: Boolean
    val interface: String
    val port: Int
    val corsAllowedOrigins: HttpOriginMatcher
    val rateLimit: RateLimitConfig
  }

  object JsonRpcHttpServerConfig {
    def apply(mantisConfig: TypesafeConfig): JsonRpcHttpServerConfig = {
      val rpcHttpConfig = mantisConfig.getConfig("network.rpc.http")

      new JsonRpcHttpServerConfig {
        override val mode: String = rpcHttpConfig.getString("mode")
        override val enabled: Boolean = rpcHttpConfig.getBoolean("enabled")
        override val interface: String = rpcHttpConfig.getString("interface")
        override val port: Int = rpcHttpConfig.getInt("port")

        override val corsAllowedOrigins = ConfigUtils.parseCorsAllowedOrigins(rpcHttpConfig, "cors-allowed-origins")

        override val rateLimit = RateLimitConfig(rpcHttpConfig.getConfig("rate-limit"))
      }
    }
  }
}
