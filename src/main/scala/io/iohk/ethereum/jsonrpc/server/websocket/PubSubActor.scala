package io.iohk.ethereum.jsonrpc.server.websocket

import java.security.SecureRandom

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.SourceQueueWithComplete
import io.iohk.ethereum.crypto
import io.iohk.ethereum.eventbus.event.NewHead
import io.iohk.ethereum.jsonrpc._
import org.json4s.{Extraction, JValue}
import org.json4s.JsonAST.{JObject, JString}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}
import org.spongycastle.util.encoders.Hex

import scala.util.Try

object PubSubActor {
  def props: Props = Props(new PubSubActor)

  case class JsonRpcSubscriptionNotification(subscription: String, result: JValue) {
    val jsonrpc = "2.0"
    val method = "eth_subscription"
    val params = JObject("subscription" -> JString(subscription))
  }

  case class Subscription(id: String, `type`: SubscriptionType, out: SourceQueueWithComplete[Message])
  sealed trait SubscriptionType
  case class NewHeads(includeTransactions: Boolean) extends SubscriptionType

  case class Subscribe(connectionId: String, req: JsonRpcRequest, out: SourceQueueWithComplete[Message])
  case class Unsubscribe(connectionId: String, req: JsonRpcRequest)
  case class UnsubscribeAll(connectionId: String)
}

class PubSubActor extends Actor with JsonMethodsImplicits {

  import PubSubActor._

  context.system.eventStream.subscribe(self, classOf[NewHead])

  private val secureRandom = new SecureRandom

  private var subscriptions: Map[String, Set[Subscription]] = Map.empty

  override def receive: Receive = {
    case Subscribe(connectionId, req, out) =>
      val subscriptionId = Hex.toHexString(crypto.secureRandomByteArray(secureRandom, 16))

      val existingSubscriptions = subscriptions.getOrElse(connectionId, Set.empty)

      Try(req.params.get.arr.head.extract[String]).toOption match {
        case Some("newHeads") =>
          val params = req.params.get.arr
          val includeTransactions =
            if (params.length > 1) (params(1) \ "includeTransactions").extractOpt[Boolean].getOrElse(false)
            else false
          val newSubscription = Subscription(subscriptionId, NewHeads(includeTransactions), out)
          subscriptions += (connectionId -> (existingSubscriptions + newSubscription))
          out.offer(toMessage(JsonRpcResponse("2.0", Some(subscriptionId), None, req.id)))

        case _ =>
          val response = JsonRpcResponse("2.0", None, Some(JsonRpcErrors.InvalidParams("Subscription not supported: " + req.params)), req.id)
          out.offer(toMessage(response))
      }

    case Unsubscribe(connectionId, req) =>
      Try(req.params.get.arr.head.extract[String]).toOption.foreach { subscriptionId =>
        val existingSubscriptions = subscriptions.getOrElse(connectionId, Set.empty)
        val subscriptionToRemoveOpt = existingSubscriptions.find(_.id == subscriptionId)
        subscriptions += (connectionId -> existingSubscriptions.filterNot(_.id == subscriptionId))
        subscriptionToRemoveOpt.foreach { removedSubscription =>
          val response = JsonRpcResponse("2.0", Some(true), None, req.id)
          removedSubscription.out.offer(toMessage(response))
        }
      }

    case UnsubscribeAll(connectionId) =>
      subscriptions -= connectionId

    case NewHead(block) =>
      subscriptions.values.flatten
        .collect { case Subscription(id, NewHeads(includeTransactions), out) => (id, includeTransactions, out) }
        .foreach { case (id, includeTransactions, out) =>
          val blockResponse = BlockResponse(block, None, fullTxs = includeTransactions)
          val response = JsonRpcSubscriptionNotification(id, Extraction.decompose(blockResponse))
          out.offer(toMessage(response))
      }
  }

  private def toMessage(any: Any): TextMessage = {
    TextMessage(compact(render(Extraction.decompose(any))))
  }
}
