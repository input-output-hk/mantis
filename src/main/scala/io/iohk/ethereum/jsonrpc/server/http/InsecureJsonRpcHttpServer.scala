package io.iohk.ethereum.jsonrpc.server.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.RemoteAddress
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import com.twitter.util.LruMap
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class InsecureJsonRpcHttpServer(
    val jsonRpcController: JsonRpcBaseController,
    val jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig
)(implicit val actorSystem: ActorSystem)
    extends JsonRpcHttpServer
    with Logger {

  override val ipTrackingEnabled: Boolean = config.ipTrackingEnabled

  override val minRequestInterval: FiniteDuration = config.minRequestInterval

  override val latestTimestampCacheSize: Int = config.latestTimestampCacheSize

  override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)

  def run(): Unit = {
    val bindingResultF = Http(actorSystem).newServerAt(config.interface, config.port).bind(route)

    bindingResultF onComplete {
      case Success(serverBinding) => log.info(s"JSON RPC HTTP server listening on ${serverBinding.localAddress}")
      case Failure(ex) => log.error("Cannot start JSON HTTP RPC server", ex)
    }
  }

  override def corsAllowedOrigins: HttpOriginMatcher = config.corsAllowedOrigins
}
