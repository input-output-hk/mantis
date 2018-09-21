package io.iohk.ethereum.jsonrpc.server.websocket

import java.security.SecureRandom

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Address, Block, Blockchain}
import io.iohk.ethereum.eventbus.event.NewHead
import io.iohk.ethereum.jsonrpc.FilterManager.TxLog
import io.iohk.ethereum.jsonrpc._
import io.iohk.ethereum.utils.ByteUtils
import org.json4s.{Extraction, JValue}
import org.json4s.JsonAST.{JObject, JString}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}
import org.spongycastle.util.encoders.Hex

import scala.util.Try

object PubSubActor {
  def props(blockchain: Blockchain): Props = Props(new PubSubActor(blockchain))

  val JsonRpcVersion = "2.0"

  case class JsonRpcSubscriptionNotification(subscription: String, result: JValue) {
    val jsonrpc: String = JsonRpcVersion
    val method: String = "eth_subscription"
    val params: JObject = JObject("subscription" -> JString(subscription))
  }

  case class Subscription(id: String, `type`: SubscriptionType, out: SourceQueueWithComplete[Message])
  sealed trait SubscriptionType
  case class NewHeads(includeTransactions: Boolean) extends SubscriptionType
  case class Logs(address: Option[Address], topics: Option[Seq[ByteString]]) extends SubscriptionType

  case class Subscribe(connectionId: String, req: JsonRpcRequest, out: SourceQueueWithComplete[Message])
  case class Unsubscribe(connectionId: String, req: JsonRpcRequest)
  case class UnsubscribeAll(connectionId: String)

  val NewHeadsSubscription = "newHeads"
  val LogsSubscription = "logs"
}

class PubSubActor(blockchain: Blockchain) extends Actor with JsonMethodsImplicits {

  import PubSubActor._

  context.system.eventStream.subscribe(self, classOf[NewHead])

  private val secureRandom = new SecureRandom

  private var subscriptions: Map[String, Set[Subscription]] = Map.empty

  override def receive: Receive = {
    case Subscribe(connectionId, req, out) => handleSubscribe(connectionId, req, out)
    case Unsubscribe(connectionId, req) => handleUnsubscribe(connectionId, req)
    case UnsubscribeAll(connectionId) => subscriptions -= connectionId
    case NewHead(blocksRemoved, blocksAdded) => handleNewHead(blocksRemoved, blocksAdded)
  }

  private def handleSubscribe(connectionId: String, req: JsonRpcRequest, out: SourceQueueWithComplete[Message]): Unit = {
    val subscriptionId = Hex.toHexString(crypto.secureRandomByteArray(secureRandom, 16))
    val existingSubscriptions = subscriptions.getOrElse(connectionId, Set.empty)
    val params = req.params.get.arr

    Try(params.head.extract[String]).toOption match {
      case Some(NewHeadsSubscription) =>
        val includeTransactions =
          if (params.length > 1) (params(1) \ "includeTransactions").extractOpt[Boolean].getOrElse(false)
          else false
        val newSubscription = Subscription(subscriptionId, NewHeads(includeTransactions), out)
        subscriptions += (connectionId -> (existingSubscriptions + newSubscription))
        out.offer(toMessage(JsonRpcResponse(JsonRpcVersion, Some(subscriptionId), None, req.id)))

      case Some(LogsSubscription) =>
        val (address, topics) = if (params.length > 1) {
          val rawAddress = (params(1) \ "address").extractOpt[String]
          val rawTopics = (params(1) \ "topics").extractOpt[Seq[String]]
          (rawAddress.map(Address.apply),
            rawTopics.map(_.map(s => ByteUtils.padLeft(ByteString(Hex.decode(s.replace("0x", ""))), 32))))
        } else (None, None)
        val newSubscription = Subscription(subscriptionId, Logs(address, topics), out)
        subscriptions += (connectionId -> (existingSubscriptions + newSubscription))
        out.offer(toMessage(JsonRpcResponse(JsonRpcVersion, Some(subscriptionId), None, req.id)))

      case _ =>
        val response = JsonRpcResponse(JsonRpcVersion, None, Some(JsonRpcErrors.InvalidParams("Subscription not supported: " + req.params)), req.id)
        out.offer(toMessage(response))
    }
  }

  private def handleUnsubscribe(connectionId: String, req: JsonRpcRequest): Unit = {
    Try(req.params.get.arr.head.extract[String]).toOption.foreach { subscriptionId =>
      val existingSubscriptions = subscriptions.getOrElse(connectionId, Set.empty)
      val subscriptionToRemoveOpt = existingSubscriptions.find(_.id == subscriptionId)
      subscriptions += (connectionId -> existingSubscriptions.filterNot(_.id == subscriptionId))
      subscriptionToRemoveOpt.foreach { removedSubscription =>
        val response = JsonRpcResponse(JsonRpcVersion, Some(true), None, req.id)
        removedSubscription.out.offer(toMessage(response))
      }
    }
  }

  private def handleNewHead(blocksRemoved: Seq[Block], blocksAdded: Seq[Block]): Unit = {
    processNewHeadsSubscriptions(blocksAdded)
    processRemovedLogs(blocksRemoved)
    processNewLogs(blocksAdded)
  }

  private def processNewHeadsSubscriptions(blocksAdded: Seq[Block]): Unit = {
    blocksAdded.foreach { block =>
      subscriptions.values.flatten
        .collect { case Subscription(id, NewHeads(includeTransactions), out) => (id, includeTransactions, out) }
        .foreach { case (id, includeTransactions, out) =>
          val blockResponse = BlockResponse(block, None, fullTxs = includeTransactions)
          val response = JsonRpcSubscriptionNotification(id, Extraction.decompose(blockResponse))
          out.offer(toMessage(response))
        }
    }
  }

  private def processRemovedLogs(blocksRemoved: Seq[Block]): Unit = {
    processTxLogs(blocksRemoved, removed = true)
  }

  private def processNewLogs(blocksAdded: Seq[Block]): Unit = {
    processTxLogs(blocksAdded, removed = false)
  }

  private def processTxLogs(blocks: Seq[Block], removed: Boolean): Unit = {
    val txLogs = prepareTxLogs(blocks, removed)
    txLogs.foreach { txLog =>
      subscriptions.values.flatten
        .collect { case Subscription(id, logs: Logs, out) if logMatches(logs, txLog) => (id, out) }
        .foreach { case (id, out) =>
          val response = JsonRpcSubscriptionNotification(id, Extraction.decompose(txLog))
          out.offer(toMessage(response))
        }
    }
  }

  private def prepareTxLogs(blocks: Seq[Block], removed: Boolean): Seq[TxLog] = blocks.flatMap { block =>
    val txList = block.body.transactionList
    val receipts = blockchain.getReceiptsByHash(block.header.hash).getOrElse(Nil)
    (txList zip receipts).zipWithIndex.flatMap { case ((tx, receipt), txIndex) =>
      receipt.logs.zipWithIndex.map { case (log, logIndex) =>
        TxLog(
          logIndex = logIndex,
          transactionIndex = txIndex,
          transactionHash = tx.hash,
          blockHash = block.header.hash,
          blockNumber = block.header.number,
          address = log.loggerAddress,
          data = log.data,
          topics = log.logTopics,
          removed = removed)
      }
    }
  }

  private def logMatches(logs: Logs, log: TxLog): Boolean = {
    logs.address.forall(_ == log.address) &&
      logs.topics.forall(topics => log.topics.length >= topics.length) &&
      logs.topics.forall { topics => (topics zip log.topics).forall { case (a, b) => a == b } }
  }

  private def toMessage(any: Any): TextMessage = {
    TextMessage(compact(render(Extraction.decompose(any))))
  }
}
