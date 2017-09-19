package io.iohk.ethereum.jsonrpc.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.server.JsonRpcServer.JsonRpcServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcResponse}
import org.json4s.JsonAST.{JInt, JString}
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

import scala.concurrent.Future

class JsonRpcServerSpec extends FlatSpec with Matchers with ScalatestRouteTest {

  it should "pass valid json request to controller" in new TestSetup {
    (mockJsonRpcController.handleRequest _).expects(*).returning(Future.successful(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest = HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~>  Route.seal(mockJsonRpcServer.route) ~> check {
      status === StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  it should "pass valid batch json request to controller" in new TestSetup {
    (mockJsonRpcController.handleRequest _).expects(*)
      .twice()
      .returning(Future.successful(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""[{"jsonrpc":"2.0", "method": "asd", "id": "1"}, {"jsonrpc":"2.0", "method": "asd", "id": "2"}]""")
    val postRequest = HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~>  Route.seal(mockJsonRpcServer.route) ~> check {
      status === StatusCodes.OK
      responseAs[String] shouldEqual """[{"jsonrpc":"2.0","result":"this is a response","id":1},{"jsonrpc":"2.0","result":"this is a response","id":1}]"""
    }
  }

  it should "return BadRequest when malformed request is received" in new TestSetup {
    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "this is not a valid json""")
    val postRequest = HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~>  Route.seal(mockJsonRpcServer.route) ~> check {
      status === StatusCodes.BadRequest
    }
  }

  trait TestSetup extends MockFactory {
    val config = new JsonRpcServerConfig {
      override val mode: String = "mockJsonRpc"
      override val enabled: Boolean = true
      override val interface: String = ""
      override val port: Int = 123
      override val certificateKeyStorePath = None
      override val certificatePasswordFile = None
    }

    val mockJsonRpcController = mock[JsonRpcController]
    val mockJsonRpcServer = new JsonRpcServer {
      val jsonRpcController = mockJsonRpcController

      def run(): Unit = ()
    }
  }

}
