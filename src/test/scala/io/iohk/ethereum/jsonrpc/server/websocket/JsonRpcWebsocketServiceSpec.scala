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
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.eventbus.event.NewHead
import org.json4s.JsonAST.{JArray, JString}

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

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage("""{"jsonrpc":"2.0", "method": "eth_subscribe" "params": ["newHeads", {}], "id": "1"}""")
      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(block))
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

    WS("/", wsClient.flow) ~> jsonRpcWebsocketService.websocketRoute ~> check {
      isWebSocketUpgrade shouldEqual true

      wsClient.sendMessage("""{"jsonrpc":"2.0", "method": "eth_subscribe" "params": ["newHeads", {"includeTransactions": true}], "id": "1"}""")
      val subscriptionMsg = wsClient.expectMessage()
      val subscriptionId = (parse(subscriptionMsg.asTextMessage.getStrictText) \ "result").extract[String]

      system.eventStream.publish(NewHead(block))
      val notificationMsg = wsClient.expectMessage()

      val parsedNotification = parse(notificationMsg.asTextMessage.getStrictText)
      (parsedNotification \ "subscription").extract[String] shouldBe subscriptionId
      BigInt((parsedNotification \ "result" \ "number").extract[String].drop(2), 16) shouldBe block.header.number
      ((parsedNotification \ "result" \ "transactions").extract[JArray].arr.head \ "nonce").extract[String] shouldBe "0x6b116"

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
    val jsonRpcWebsocketService = new JsonRpcWebsocketServer(jsonRpcController, config)
    val wsClient = WSProbe()
  }

}
