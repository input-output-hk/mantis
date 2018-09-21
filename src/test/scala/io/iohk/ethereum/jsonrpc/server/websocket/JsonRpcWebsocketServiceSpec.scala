package io.iohk.ethereum.jsonrpc.server.websocket

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.WSProbe
import io.iohk.ethereum.jsonrpc.{JsonMethodsImplicits, JsonRpcController, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.jsonrpc.server.websocket.JsonRpcWebsocketServer.JsonRpcWebsocketServerConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.json4s.native.JsonMethods._
import akka.http.scaladsl.testkit._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.{Address, BlockchainImpl, Receipt, TxLogEntry}
import io.iohk.ethereum.eventbus.event.NewHead
import io.iohk.ethereum.utils.ByteUtils
import org.json4s.JsonAST.{JArray, JString}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future

class JsonRpcWebsocketServiceSpec extends FlatSpec with Matchers with MockFactory with ScalatestRouteTest {

  "JsonRpcWebsocketService" should "handle JSON RPC requests" in new TestSetup {
    val expectedRequest = JsonRpcRequest("2.0", "eth_test", None, Some(JString("1")))
    val response = JsonRpcResponse("2.0", Some(JString("test response")), None, JString("1"))

    (jsonRpcController.handleRequest _).expects(expectedRequest).returns(Future.successful(response))

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage("""{"jsonrpc":"2.0", "method": "eth_test", "id": "1"}""")
      wsClient.expectMessage("""{"jsonrpc":"2.0","result":"test response","id":"1"}""")

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }

  it should "handle newHeads subscription without transactions" in new TestSetup {
    val block = Fixtures.Blocks.Block3125369.block

    (blockchain.getReceiptsByHash _).expects(block.header.hash).returning(Some(Nil))

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage("""{"jsonrpc":"2.0", "method": "eth_subscribe" "params": ["newHeads", {}], "id": "1"}""")
      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(Nil, Seq(block)))
      val notificationMsg = wsClient.expectMessage()

      val parsedNotification = parse(notificationMsg.asTextMessage.getStrictText)
      (parsedNotification \ "subscription").extract[String] shouldBe subscriptionId
      BigInt((parsedNotification \ "result" \ "number").extract[String].drop(2), 16) shouldBe block.header.number
      (parsedNotification \ "result" \ "transactions").extract[JArray].arr.head.extract[String] shouldBe "0xaf854c57c64191827d1c80fc50f716f824508973e12e4d4c60d270520ce72edb"

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }

  it should "handle newHeads subscription with full transactions" in new TestSetup {
    val block = Fixtures.Blocks.Block3125369.block

    (blockchain.getReceiptsByHash _).expects(block.header.hash).returning(Some(Nil))

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage("""{"jsonrpc":"2.0", "method": "eth_subscribe" "params": ["newHeads", {"includeTransactions": true}], "id": "1"}""")
      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(Nil, Seq(block)))
      val notificationMsg = wsClient.expectMessage()

      val parsedNotification = parse(notificationMsg.asTextMessage.getStrictText)
      (parsedNotification \ "subscription").extract[String] shouldBe subscriptionId
      BigInt((parsedNotification \ "result" \ "number").extract[String].drop(2), 16) shouldBe block.header.number
      ((parsedNotification \ "result" \ "transactions").extract[JArray].arr.head \ "nonce").extract[String] shouldBe "0x6b116"

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }

  it should "handle logs subscription" in new TestSetup {
    val block = Fixtures.Blocks.Block3125369.block

    val subscriptionAddress = Address(Hex.decode("1234"))
    val anotherAddress = Address(Hex.decode("123456"))
    val subscriptionTopic = ByteUtils.padLeft(ByteString(Hex.decode("00FF")), 32)
    val logData = ByteString(Hex.decode("AAFF00"))
    val logs = Seq(
      TxLogEntry(subscriptionAddress, Seq(subscriptionTopic), logData), // should be returned, address and topics match
      TxLogEntry(anotherAddress, Seq(subscriptionTopic), ByteString()), // should not be returned, address does not match
      TxLogEntry(subscriptionAddress, Seq(), ByteString())) // should not be returned, topics do not match
    val receipts = Seq(Receipt(ByteString(), 0, ByteString(), logs, None, None))
    (blockchain.getReceiptsByHash _).expects(block.header.hash).returning(Some(receipts))

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage(
        s"""{
           |"jsonrpc":"2.0",
           |"method": "eth_subscribe",
           |"params": ["logs", {"address": "${subscriptionAddress.toString}", "topics": ["${Hex.toHexString(subscriptionTopic.toArray[Byte])}"]}],
           |"id": "1"
         }""".stripMargin)

      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(Nil, Seq(block)))

      val notificationMsg1 = wsClient.expectMessage()
      val parsedNotification1 = parse(notificationMsg1.asTextMessage.getStrictText)
      (parsedNotification1 \ "subscription").extract[String] shouldBe subscriptionId
      (parsedNotification1 \ "result" \ "blockHash").extract[String] shouldBe s"0x${Hex.toHexString(block.header.hash.toArray[Byte])}"
      (parsedNotification1 \ "result" \ "address").extract[String] shouldBe subscriptionAddress.toString
      (parsedNotification1 \ "result" \ "data").extract[String] shouldBe s"0x${Hex.toHexString(logData.toArray[Byte])}"

      wsClient.expectNoMessage()

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }

  it should "handle logs removed logs with multiple topics" in new TestSetup {
    val block = Fixtures.Blocks.Block3125369.block

    val subscriptionAddress = Address(Hex.decode("1234"))
    val anotherAddress = Address(Hex.decode("123456"))
    val subscriptionTopic1 = ByteUtils.padLeft(ByteString(Hex.decode("00FF")), 32)
    val subscriptionTopic2 = ByteUtils.padLeft(ByteString(Hex.decode("00AA")), 32)
    val logData = ByteString(Hex.decode("AAFF00"))
    val logs = Seq(TxLogEntry(subscriptionAddress, Seq(subscriptionTopic1, subscriptionTopic2), logData))
    val receipts = Seq(Receipt(ByteString(), 0, ByteString(), logs, None, None))
    (blockchain.getReceiptsByHash _).expects(block.header.hash).returning(Some(receipts)).twice()

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage(
        s"""{
           |"jsonrpc":"2.0",
           |"method": "eth_subscribe",
           |"params": ["logs", {"address": "${subscriptionAddress.toString}", "topics": ["${Hex.toHexString(subscriptionTopic1.toArray[Byte])}"]}],
           |"id": "1"
         }""".stripMargin)

      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(Nil, Seq(block)))

      val notificationMsg1 = wsClient.expectMessage()
      val parsedNotification1 = parse(notificationMsg1.asTextMessage.getStrictText)
      (parsedNotification1 \ "subscription").extract[String] shouldBe subscriptionId
      (parsedNotification1 \ "result" \ "blockHash").extract[String] shouldBe s"0x${Hex.toHexString(block.header.hash.toArray[Byte])}"
      (parsedNotification1 \ "result" \ "address").extract[String] shouldBe subscriptionAddress.toString
      (parsedNotification1 \ "result" \ "data").extract[String] shouldBe s"0x${Hex.toHexString(logData.toArray[Byte])}"

      system.eventStream.publish(NewHead(Seq(block), Nil))

      val notificationMsg2 = wsClient.expectMessage()
      val parsedNotification2 = parse(notificationMsg2.asTextMessage.getStrictText)
      (parsedNotification2 \ "subscription").extract[String] shouldBe subscriptionId
      (parsedNotification2 \ "result" \ "blockHash").extract[String] shouldBe s"0x${Hex.toHexString(block.header.hash.toArray[Byte])}"
      (parsedNotification2 \ "result" \ "address").extract[String] shouldBe subscriptionAddress.toString
      (parsedNotification2 \ "result" \ "data").extract[String] shouldBe s"0x${Hex.toHexString(logData.toArray[Byte])}"
      (parsedNotification2 \ "result" \ "removed").extract[Boolean] shouldBe true

      wsClient.expectNoMessage()

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }

  trait TestSetup extends JsonMethodsImplicits {
    implicit val system = ActorSystem("JsonRpcWebsocketServiceSpec_System")
    implicit val materializer = ActorMaterializer()

    val jsonRpcController = mock[JsonRpcController]
    val config = new JsonRpcWebsocketServerConfig {
      override val enabled: Boolean = true
      override val interface: String = "127.0.0.1"
      override val port: Int = 9000
    }

    val blockchain = mock[BlockchainImpl]
    val jsonRpcWebsocketService = new JsonRpcWebsocketServer(jsonRpcController, blockchain, config)
    val wsClient = WSProbe()
  }

}
