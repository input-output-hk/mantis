package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.{
  OptionNoneToJNullSerializer,
  QuantitiesSerializer,
  UnformattedDataJsonSerializer
}
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.{Fixtures, LongPatience, WithActorSystemShutDown}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.json4s.{DefaultFormats, Formats, JArray, JObject, JString}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class JsonRpcControllerSpec
    extends TestKit(ActorSystem("JsonRpcControllerSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  "JsonRpcController" should "handle valid sha3 request" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_sha3", JString("0x1234") :: Nil)

    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult("0x56570de287d73cd1cb6092bb8fdee6173974955fdef345ae579ee9f475ea7432")
  }

  it should "fail when invalid request is received" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_sha3", JString("asdasd") :: Nil)

    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveError(JsonRpcError.InvalidParams("Invalid method parameters"))
  }

  it should "handle clientVersion request" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_clientVersion")

    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult(version)
  }

  it should "Handle net_peerCount request" in new JsonRpcControllerFixture {
    (netService.peerCount _).expects(*).returning(Task.now(Right(PeerCountResponse(123))))

    val rpcRequest = newJsonRpcRequest("net_peerCount")

    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult("0x7b")
  }

  it should "Handle net_listening request" in new JsonRpcControllerFixture {
    (netService.listening _).expects(*).returning(Task.now(Right(ListeningResponse(false))))

    val rpcRequest = newJsonRpcRequest("net_listening")
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveBooleanResult(false)
  }

  it should "Handle net_version request" in new JsonRpcControllerFixture {
    val netVersion = "99"

    (netService.version _).expects(*).returning(Task.now(Right(VersionResponse(netVersion))))

    val rpcRequest = newJsonRpcRequest("net_version")
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult(netVersion)
  }

  it should "only allow to call methods of enabled apis" in new JsonRpcControllerFixture {
    override def config: JsonRpcConfig = new JsonRpcConfig {
      override val apis = Seq("web3")
      override val accountTransactionsMaxBlocks = 50000
      override def minerActiveTimeout: FiniteDuration = ???
      override def httpServerConfig: JsonRpcHttpServer.JsonRpcHttpServerConfig = ???
      override def ipcServerConfig: JsonRpcIpcServer.JsonRpcIpcServerConfig = ???
      override def healthConfig: NodeJsonRpcHealthChecker.JsonRpcHealthConfig = ???
    }

    val ethRpcRequest = newJsonRpcRequest("eth_protocolVersion")
    val ethResponse = jsonRpcController.handleRequest(ethRpcRequest).runSyncUnsafe()

    ethResponse should haveError(JsonRpcError.MethodNotFound)

    val web3RpcRequest = newJsonRpcRequest("web3_clientVersion")
    val web3Response = jsonRpcController.handleRequest(web3RpcRequest).runSyncUnsafe()

    web3Response should haveStringResult(version)
  }

  it should "debug_listPeersInfo" in new JsonRpcControllerFixture {
    val peerStatus = RemoteStatus(
      protocolVersion = ProtocolVersions.PV63,
      networkId = 1,
      chainWeight = ChainWeight.totalDifficultyOnly(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = true,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )
    val peers = List(initialPeerInfo)

    (debugService.listPeersInfo _)
      .expects(ListPeersInfoRequest())
      .returning(Task.now(Right(ListPeersInfoResponse(peers))))

    val rpcRequest = newJsonRpcRequest("debug_listPeersInfo")
    val response: JsonRpcResponse = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveResult(JArray(peers.map(info => JString(info.toString))))
  }

  it should "rpc_modules" in new JsonRpcControllerFixture {
    val request: JsonRpcRequest = newJsonRpcRequest("rpc_modules")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    response should haveResult(
      JObject(
        "net" -> JString("1.0"),
        "rpc" -> JString("1.0"),
        "personal" -> JString("1.0"),
        "eth" -> JString("1.0"),
        "web3" -> JString("1.0"),
        "mantis" -> JString("1.0"),
        "debug" -> JString("1.0"),
        "qa" -> JString("1.0"),
        "checkpointing" -> JString("1.0")
      )
    )
  }
}
