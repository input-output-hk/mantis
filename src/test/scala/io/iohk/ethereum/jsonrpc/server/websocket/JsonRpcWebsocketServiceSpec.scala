package io.iohk.ethereum.jsonrpc.server.websocket

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.WSProbe
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.jsonrpc.server.websocket.JsonRpcWebsocketServer.JsonRpcWebsocketServerConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import akka.http.scaladsl.testkit._
import akka.stream.ActorMaterializer
import org.json4s.JsonAST.JString
import scala.concurrent.Future

class JsonRpcWebsocketServiceSpec extends FlatSpec with Matchers with MockFactory with ScalatestRouteTest {

  "JsonRpcWebsocketService" should "handle JSON RPC requests" in {
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

}
