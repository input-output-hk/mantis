package io.iohk.ethereum.jsonrpc.server.http

import java.io.{PrintWriter, StringWriter}
import java.security.SecureRandom
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.javadsl.CorsRejection
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.buildinfo.MantisBuildInfo
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.utils.{ConfigUtils, JsonUtils, Logger, Riemann}
import io.riemann.riemann.client.EventDSL
import org.json4s.JsonAST.JInt
import org.json4s.{DefaultFormats, native}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

trait JsonRpcHttpServer extends Json4sSupport with Logger {
  val jsonRpcController: JsonRpcController

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats

  def corsAllowedOrigins: HttpOriginRange

  val corsSettings = CorsSettings.defaultSettings.copy(
    allowGenericHttpRequests = true,
    allowedOrigins = corsAllowedOrigins
  )

  protected def exception2string(t: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw, true)
    t.printStackTrace(pw)
    sw.toString
  }

  protected def addRequestAttributes(event: EventDSL, request: HttpRequest)(implicit actorSystem: ActorSystem): EventDSL = {
    val entity: RequestEntity = request.entity

    request.copy()

    implicit val materializer = ActorMaterializer()
    // val entityF: Future[String] = Unmarshaller.stringUnmarshaller(entity)

    val entityStrictF: Future[HttpEntity.Strict] = entity.toStrict(FiniteDuration(10L * 100L, TimeUnit.MILLISECONDS))

    entityStrictF.andThen {
      case Success(entityStrict) ⇒
        val data = entityStrict.getData()
        val utf8 = HttpCharsets.`UTF-8`
        val charset = entityStrict.contentType.charsetOption.getOrElse(utf8)

          event
          .attribute("request", String.valueOf(request))
          .attribute("request-entity", String.valueOf(entity))
          .attribute("request-entity-strict", entityStrict.toString())
          .attribute("request-entity-string", data.decodeString(charset.nioCharset()))

      case Failure(exception) ⇒
        event.attribute("entity-strict-error", exception2string(exception))
        log.error("While transforming request entity to string", exception)
    }

    event
  }

  protected def logRequest(requestContext: RequestContext, rejected: Boolean)(implicit actorSystem: ActorSystem): Unit = {
    val request: HttpRequest = requestContext.request
    val service = if(rejected) "rpc route rejected" else "rpc route"

    addRequestAttributes(Riemann.warning(service), request).send()
  }

  implicit def myRejectionHandler(request: HttpRequest)(implicit actorSystem: ActorSystem): RejectionHandler =
    RejectionHandler.newBuilder()
      .handle {
        case r: MalformedRequestContentRejection =>
          val rJson = JsonUtils.pretty(r)

          val event = Riemann
            .warning("rpc rejection handler [MalformedRequestContentRejection]")
            .attribute("rejection", r.toString)
            .attribute("rejectionJson", rJson)
            .attribute("exception", exception2string(r.cause))
            .description("aaa")

          addRequestAttributes(event, request).send()

          complete((StatusCodes.BadRequest, JsonRpcResponse("2.0", None, Some(JsonRpcErrors.ParseError), JInt(0))))

        case r: CorsRejection =>
          val rJson = JsonUtils.pretty(r)

          val event = Riemann
            .warning("rpc rejection handler [CorsRejection]")
            .attribute("rejection", r.toString)
            .attribute("rejectionJson", rJson)
            .attribute("origin", r.getOrigin.map[String](JsonUtils.pretty(_)).orElse(""))
            .attribute("method", r.getMethod.map[String](JsonUtils.pretty(_)).orElse(""))
            .attribute("headers", r.getHeaders.map[String](JsonUtils.pretty(_)).orElse("[]"))
            .description("aaa")

          addRequestAttributes(event, request).send()

          complete(StatusCodes.Forbidden)
      }
      .result()

  protected def _handleRejections(requestContext: RequestContext, handler: RejectionHandler)(implicit actorSystem: ActorSystem): Directive0 = {
    val handled = handleRejections(handler)
    handled.andThen { _ ⇒ logRequest(requestContext, true) }
    handled
  }

  protected def routeCtx(requestContext: RequestContext)(implicit actorSystem: ActorSystem): Route = cors(corsSettings) {
    logRequest(requestContext, false)

    _handleRejections(requestContext, myRejectionHandler(requestContext.request))(actorSystem) {
      (path("healthcheck") & pathEndOrSingleSlash & get) {
        handleHealthcheck()
      } ~
        (path("buildinfo") & pathEndOrSingleSlash & get) {
          handleBuildInfo()
        } ~
        (pathEndOrSingleSlash & post) {
          entity(as[JsonRpcRequest]) { parsedEntity =>
            val peJson = JsonUtils.pretty(parsedEntity)
            val event = Riemann
              .warning("rpc route entity JsonRpcRequest")
              .attribute("type", "JsonRpcRequest")
              .attribute("parsedEntity", parsedEntity.toString)
              .attribute("parsedEntityJson", peJson)
              .description("aaa")

            addRequestAttributes(event, requestContext.request).send()

            handleRequest(parsedEntity)
          } ~ entity(as[Seq[JsonRpcRequest]]) { parsedEntities =>
            val pesJson = JsonUtils.pretty(parsedEntities)
            val event = Riemann
              .warning("rpc route entity Seq[JsonRpcRequest]")
              .attribute("type", "Seq[JsonRpcRequest]")
              .attribute("parsedEntities", parsedEntities.toString())
              .attribute("parsedEntitiesJson", pesJson)
              .description("aaa")

            addRequestAttributes(event, requestContext.request).send()

            handleBatchRequest(parsedEntities)
          }
        }
    }
  }

  def route(implicit actorSystem: ActorSystem): Route =
    extractRequestContext.tapply {
      case Tuple1(requestContext) ⇒
        extractStrictEntity(FiniteDuration(1 * 1, TimeUnit.SECONDS)).tapply {
          case Tuple1(entity) ⇒
            routeCtx(requestContext)
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
