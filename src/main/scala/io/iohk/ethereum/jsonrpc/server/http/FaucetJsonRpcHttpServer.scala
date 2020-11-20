package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom
import java.time.Clock

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import com.twitter.util.LruMap
import io.iohk.ethereum.faucet.jsonrpc.FaucetJsonRpcController
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcHealthChecker, JsonRpcRequest}
import io.iohk.ethereum.utils.Logger
import monix.execution.Scheduler.Implicits.global

trait FaucetJsonRpcHttpServer extends JsonRpcHttpServer {

  val minRequestInterval: Int

  val latestTimestampCacheSize: Int

  val latestRequestTimestamps: LruMap[RemoteAddress, Long]

  val clock: Clock = Clock.systemUTC()

  override def route: Route = cors(corsSettings) {
    (pathEndOrSingleSlash & post) {
      (extractClientIP & entity(as[JsonRpcRequest])) { (clientAddress: RemoteAddress, request: JsonRpcRequest) =>
        handleRequest(clientAddress, request)
      }
    }
  }

  def handleRequest(clientAddr: RemoteAddress, request: JsonRpcRequest): StandardRoute = {
    val timeMillis = clock.instant().toEpochMilli

    if (request.method == FaucetJsonRpcController.SendFunds) {
      val latestRequestTimestamp = latestRequestTimestamps.getOrElse(clientAddr, 0L)
      if (latestRequestTimestamp + minRequestInterval < timeMillis) {
        latestRequestTimestamps.put(clientAddr, timeMillis)
        complete(jsonRpcController.handleRequest(request).runToFuture)
      } else {
        complete(StatusCodes.TooManyRequests)
      }
    } else complete(jsonRpcController.handleRequest(request).runToFuture)
  }
}

object FaucetJsonRpcHttpServer extends Logger {
  def apply(
      jsonRpcController: JsonRpcBaseController,
      jsonRpcHealthchecker: JsonRpcHealthChecker,
      config: JsonRpcHttpServerConfig,
      secureRandom: SecureRandom
  )(implicit actorSystem: ActorSystem): Either[String, JsonRpcHttpServer] =
    config.mode match {
      case "http" =>
        Right(new FaucetBasicJsonRpcHttpServer(jsonRpcController, jsonRpcHealthchecker, config)(actorSystem))
      case "https" =>
        Right(new FaucetJsonRpcHttpsServer(jsonRpcController, jsonRpcHealthchecker, config, secureRandom)(actorSystem))
      case _ => Left(s"Cannot start JSON RPC server: Invalid mode ${config.mode} selected")
    }
}

class FaucetBasicJsonRpcHttpServer(
    jsonRpcController: JsonRpcBaseController,
    jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig
)(implicit actorSystem: ActorSystem)
    extends BasicJsonRpcHttpServer(jsonRpcController, jsonRpcHealthChecker, config)(actorSystem)
    with FaucetJsonRpcHttpServer {

  //FIXME: these 2 variables should be retrieved from a config file
  override val minRequestInterval: Int = 10000
  override val latestTimestampCacheSize: Int = 1024

  override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)
}

class FaucetJsonRpcHttpsServer(
    jsonRpcController: JsonRpcBaseController,
    jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig,
    secureRandom: SecureRandom
)(implicit actorSystem: ActorSystem)
    extends JsonRpcHttpsServer(jsonRpcController, jsonRpcHealthChecker, config, secureRandom)(actorSystem)
    with FaucetJsonRpcHttpServer {

  //FIXME: these 2 variables should be retrieved from a config file
  val minRequestInterval: Int = 10000
  val latestTimestampCacheSize: Int = 1024

  override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)
}
