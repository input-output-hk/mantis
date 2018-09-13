package io.iohk.ethereum.jsonrpc.server.websocket

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.ws.Message
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import io.iohk.ethereum.jsonrpc.server.websocket.JsonRpcWebsocketServer.JsonRpcWebsocketServerConfig
import io.iohk.ethereum.jsonrpc.JsonRpcController
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class JsonRpcWebsocketServer(jsonRpcController: JsonRpcController, config: JsonRpcWebsocketServerConfig)
                            (implicit ec: ExecutionContext, system: ActorSystem) extends Logger {

  import JsonRpcWebsocketServer._

  private implicit val materializer = ActorMaterializer()

  private val pubSubActor = system.actorOf(PubSubActor.props)

  private var serverBinding: Option[ServerBinding] = None

  private def webSocketService(): Flow[Message, Message, NotUsed] = {
    val websocketHandlerActor = system.actorOf(WebsocketHandlerActor.props(jsonRpcController, pubSubActor))

    val sink = Sink.actorRef[Message](websocketHandlerActor, WebsocketHandlerActor.ConnectionClosed)
    val source = Source.queue[Message](MessageQueueBufferSize, OverflowStrategy.dropTail)
    Flow.fromSinkAndSourceMat(sink, source)(Keep.right)
      .mapMaterializedValue { out =>
        websocketHandlerActor ! WebsocketHandlerActor.Init(out)
        NotUsed
      }
  }

  private[websocket] val websocketRoute =
    pathEndOrSingleSlash {
      handleWebSocketMessages(webSocketService())
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

  val MessageQueueBufferSize = 256

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
