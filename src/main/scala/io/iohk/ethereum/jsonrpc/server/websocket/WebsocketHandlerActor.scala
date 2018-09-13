package io.iohk.ethereum.jsonrpc.server.websocket

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, SourceQueueWithComplete}
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcErrors, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger
import org.json4s.JsonAST.JString
import org.json4s.{DefaultFormats, Extraction}
import org.json4s.native.JsonMethods.{compact, parse, render}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object WebsocketHandlerActor {
  def props(jsonRpcController: JsonRpcController, pubSubActor: ActorRef)
           (implicit ec: ExecutionContext, materializer: Materializer): Props =
    Props(new WebsocketHandlerActor(jsonRpcController, pubSubActor))

  case class Init(out: SourceQueueWithComplete[Message])
  case object ConnectionClosed

  private val SubscribeMethod = "eth_subscribe"
  private val UnsubscribeMethod = "eth_unsubscribe"
}

class WebsocketHandlerActor(jsonRpcController: JsonRpcController, pubSubActor: ActorRef)
                           (implicit ec: ExecutionContext, materializer: Materializer)
  extends Actor with Logger {

  import WebsocketHandlerActor._

  private implicit val formats = DefaultFormats

  private val connectionId = UUID.randomUUID().toString

  override def receive: Receive = {
    case Init(out) =>
      context become active(out)
  }

  def active(out: SourceQueueWithComplete[Message]): Receive = {
    case message: Message => handleMessage(out, message)
    case ConnectionClosed =>
      pubSubActor ! PubSubActor.UnsubscribeAll(connectionId)
      out.complete()
  }

  def handleMessage(out: SourceQueueWithComplete[Message], message: Message): Unit = message match {
    case tm: TextMessage.Strict => // only strict messages are supported
      tryExtractRequest(tm.text) match {
        case Success(req) =>
          req.method match {
            case SubscribeMethod => pubSubActor ! PubSubActor.Subscribe(connectionId, req, out)
            case UnsubscribeMethod => pubSubActor ! PubSubActor.Unsubscribe(connectionId, req)
            case _ => jsonRpcController.handleRequest(req).map { res => out.offer(responseToMessage(res)) }
          }

        case Failure(ex) =>
          log.debug("Cannot process JSON RPC request", ex)
          val response = JsonRpcResponse("2.0", None, Some(JsonRpcErrors.InvalidRequest), JString("unknown"))
          out.offer(responseToMessage(response))
      }
    case other => other.asBinaryMessage.getStreamedData.runWith(Sink.ignore, materializer)
  }

  private def tryExtractRequest(raw: String): Try[JsonRpcRequest] = {
    Try(parse(raw).extract[JsonRpcRequest])
  }

  private def responseToMessage(response: JsonRpcResponse): TextMessage = {
    TextMessage(compact(render(Extraction.decompose(response))))
  }
}
