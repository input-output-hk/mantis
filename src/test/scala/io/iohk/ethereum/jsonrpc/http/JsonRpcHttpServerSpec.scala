package io.iohk.ethereum.jsonrpc.http

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcResponse}
import io.iohk.ethereum.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import org.json4s.JsonAST.{JInt, JString}
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

import scala.concurrent.Future

class JsonRpcHttpServerSpec extends FlatSpec with Matchers with ScalatestRouteTest {

  "JsonRpcHttpServer" should "pass valid json request to controller" in new TestSetup {
    (jsonRpcController.handleRequest _).expects(*).returning(Future.successful(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest = HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~>  Route.seal(jsonRpcHttpServer.route) ~> check {
      status === StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  it should "return BadRequest when malformed request is received" in new TestSetup {
    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "this is not a valid json""")
    val postRequest = HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~>  Route.seal(jsonRpcHttpServer.route) ~> check {
      status === StatusCodes.BadRequest
    }
  }

  trait TestSetup extends MockFactory {
    val config = new JsonRpcHttpServerConfig {
      override val enabled: Boolean = true
      override val interface: String = ""
      override val port: Int = 123
    }

    val jsonRpcController = mock[JsonRpcController]
    val jsonRpcHttpServer = new JsonRpcHttpServer(jsonRpcController, config)
  }

}
