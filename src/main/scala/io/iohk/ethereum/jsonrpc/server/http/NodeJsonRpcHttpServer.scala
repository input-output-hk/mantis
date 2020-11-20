package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Route, StandardRoute}
import io.iohk.ethereum.jsonrpc.JsonRpcHealthChecker
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import akka.http.scaladsl.server.Directives._
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait NodeJsonRpcHttpServer extends JsonRpcHttpServer {

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
    complete {
      Task
        .traverse(requests)(request => jsonRpcController.handleRequest(request))
        .runToFuture
    }
  }

  override def route: Route = cors(corsSettings) {
    (path("healthcheck") & pathEndOrSingleSlash & get) {
      handleHealthcheck()
    } ~ (pathEndOrSingleSlash & post) {
      entity(as[JsonRpcRequest]) { request: JsonRpcRequest => handleRequest(request) } ~ entity(
        as[Seq[JsonRpcRequest]]
      ) { request =>
        handleBatchRequest(request)
      }
    }
  }

}

object NodeJsonRpcHttpServer extends Logger {
  def apply(
      jsonRpcController: JsonRpcBaseController,
      jsonRpcHealthchecker: JsonRpcHealthChecker,
      config: JsonRpcHttpServerConfig,
      secureRandom: SecureRandom
  )(implicit actorSystem: ActorSystem): Either[String, JsonRpcHttpServer] =
    config.mode match {
      case "http" => Right(new NodeBasicJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config)(actorSystem))
      case "https" =>
        Right(new NodeJsonRpcHttpsServer(jsonRpcController, jsonRpcHealthchecker, config, secureRandom)(actorSystem))
      case _ => Left(s"Cannot start JSON RPC server: Invalid mode ${config.mode} selected")
    }

}

class NodeBasicJsonRpcHttpServer(
    jsonRpcController: JsonRpcBaseController,
    jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig
)(implicit actorSystem: ActorSystem)
    extends BasicJsonRpcHttpServer(jsonRpcController, jsonRpcHealthChecker, config)(actorSystem)
    with NodeJsonRpcHttpServer {}

class NodeJsonRpcHttpsServer(
    jsonRpcController: JsonRpcBaseController,
    jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig,
    secureRandom: SecureRandom
)(implicit actorSystem: ActorSystem)
    extends JsonRpcHttpsServer(jsonRpcController, jsonRpcHealthChecker, config, secureRandom)(actorSystem)
    with NodeJsonRpcHttpServer {}
