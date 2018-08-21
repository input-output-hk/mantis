package io.iohk.ethereum.jsonrpc.server.websocket

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.ActorAttributes.supervisionStrategy
import akka.stream.ActorMaterializer
import akka.stream.Supervision.resumingDecider
import akka.stream.scaladsl.Flow
import io.iohk.ethereum.jsonrpc.server.websocket.JsonRpcWebsocketServer.JsonRpcWebsocketServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcErrors, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger
import org.json4s.JsonAST.JString
import org.json4s.{DefaultFormats, Extraction}
import org.json4s.native.JsonMethods.{compact, parse, render}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class JsonRpcWebsocketServer(jsonRpcController: JsonRpcController, config: JsonRpcWebsocketServerConfig)
                            (implicit ec: ExecutionContext, system: ActorSystem)
  extends Logger {

  import scala.collection.immutable._

  private implicit val materializer = ActorMaterializer()

  private implicit val formats = DefaultFormats

  private var serverBinding: Option[ServerBinding] = None

  private val webSocketService: Flow[Message, Message, NotUsed] =
    Flow[Message]
      .mapAsync[Iterable[TextMessage]](1) {
        case tm: TextMessage.Strict => // only strict messages are supported
          tryExtractRequest(tm.text) match {
            case Success(req) =>
              jsonRpcController.handleRequest(req).map { res =>
                Seq(responseToMessage(res))
              }
            case Failure(ex) =>
              log.debug("Cannot process JSON RPC request", ex)
              val response = JsonRpcResponse("2.0", None, Some(JsonRpcErrors.InvalidRequest), JString("unknown"))
              Future.successful(Seq(responseToMessage(response)))
          }
        case _ => Future.successful(Nil)
      }
      .mapConcat(identity)
      .withAttributes(supervisionStrategy(resumingDecider))

  private[websocket] val websocketRoute =
    pathEndOrSingleSlash {
      handleWebSocketMessages(webSocketService)
    }

  private def tryExtractRequest(raw: String): Try[JsonRpcRequest] = {
    Try(parse(raw).extract[JsonRpcRequest])
  }

  private def processMessage(msg: JsonRpcRequest): Future[JsonRpcResponse] = {
    jsonRpcController.handleRequest(msg)
  }

  private def responseToMessage(response: JsonRpcResponse): TextMessage = {
    TextMessage(compact(render(Extraction.decompose(response))))
  }

  def run(): Unit = {
    val bindingResultF = Http(system).bindAndHandle(websocketRoute, config.interface, config.port)

    bindingResultF onComplete {
      case Success(sb) =>
        serverBinding = Some(sb)
        log.info(s"JSON RPC websocket server listening on ${sb.localAddress}")
      case Failure(ex) => log.error("Cannot start JSON websocket RPC server", ex)
    }
  }

  def close(): Unit = {
    serverBinding.foreach(_.unbind())
  }
}

object JsonRpcWebsocketServer {
  trait JsonRpcWebsocketServerConfig {
    val enabled: Boolean
    val interface: String
    val port: Int
  }

  object JsonRpcWebsocketServerConfig {
    import com.typesafe.config.{Config ⇒ TypesafeConfig}

    def apply(mantisConfig: TypesafeConfig): JsonRpcWebsocketServerConfig = {
      val rpcHttpConfig = mantisConfig.getConfig("network.rpc.websocket")

      new JsonRpcWebsocketServerConfig {
        override val enabled: Boolean = rpcHttpConfig.getBoolean("enabled")
        override val interface: String = rpcHttpConfig.getString("interface")
        override val port: Int = rpcHttpConfig.getInt("port")
      }
    }
  }
}
