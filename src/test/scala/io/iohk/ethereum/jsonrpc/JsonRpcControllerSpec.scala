package io.iohk.ethereum.jsonrpc

import io.circe.Json.JString
import io.iohk.ethereum.jsonrpc.EthService.ProtocolVersionResponse
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class JsonRpcControllerSpec extends FlatSpec with Matchers {

  "JsonRpcController" should "handle valid sha3 request" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_sha3", Some(JArray(JString("0x1234") :: Nil)), Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x56570de287d73cd1cb6092bb8fdee6173974955fdef345ae579ee9f475ea7432"))
  }

  it should "fail when invalid request is received" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_sha3", Some(JArray(JString("asdasd") :: Nil)), Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe Some(JsonRpcErrors.InvalidParams.copy(message = "Data 'asdasd' should have 0x prefix"))
  }

  it should "handle clientVersion request" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_clientVersion", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("etc-client/v0.1"))
  }

  it should "Handle net_peerCount request" in new TestSetup {
    (netService.peerCount _).expects(*).returning(Future.successful(PeerCountResponse(123)))

    val rpcRequest = JsonRpcRequest("2.0", "net_peerCount", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.result shouldBe Some(JString("0x7b"))
  }

  it should "Handle net_listening request" in new TestSetup {
    (netService.listening _).expects(*).returning(Future.successful(ListeningResponse(false)))

    val rpcRequest = JsonRpcRequest("2.0", "net_listening", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.result shouldBe Some(JBool(false))
  }

  it should "Handle net_version request" in new TestSetup {
    (netService.version _).expects(*).returning(Future.successful(VersionResponse("99")))

    val rpcRequest = JsonRpcRequest("2.0", "net_version", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.result shouldBe Some(JString("99"))
  }

  it should "eth_protocolVersion" in new TestSetup {
    (ethService.protocolVersion _).expects(*).returning(Future.successful(ProtocolVersionResponse("0x3f")))

    val rpcRequest = JsonRpcRequest("2.0", "eth_protocolVersion", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), Duration.Inf)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x3f"))
  }

  trait TestSetup extends MockFactory {
    val web3Service = new Web3Service
    val ethService = mock[EthService]
    val netService = mock[NetService]
    val jsonRpcController = new JsonRpcController(web3Service, netService, ethService)
  }

}
