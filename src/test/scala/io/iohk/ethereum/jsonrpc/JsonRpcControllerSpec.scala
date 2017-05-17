package io.iohk.ethereum.jsonrpc

import io.circe.Json.JString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.EthService.{ProtocolVersionResponse, SyncingResponse}
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config
import org.json4s.{DefaultFormats, Extraction, Formats}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class JsonRpcControllerSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

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
    val rpcRequest = JsonRpcRequest("2.0", "eth_protocolVersion", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), Duration.Inf)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x3f"))
  }

  it should "handle eth_blockNumber request" in new TestSetup {
    val bestBlockNumber = 10
  (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val rpcRequest = JsonRpcRequest("2.0", "eth_blockNumber", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), Duration.Inf)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString(s"0xa"))
  }

  it should "eth_syncing" in new TestSetup {
    (appStateStorage.getSyncStartingBlock _).expects().returning(100)
    (appStateStorage.getBestBlockNumber _).expects().returning(200)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(300)

    val rpcRequest = JsonRpcRequest("2.0", "eth_syncing", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), Duration.Inf)

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JObject("startingBlock" -> "0x64", "currentBlock" -> "0xc8", "highestBlock" -> "0x12c"))
  }

  it should "only allow to call mehtods of enabled apis" in new TestSetup {
    override def config: JsonRpcConfig = new JsonRpcConfig { override val apis = Seq("web3") }

    val ethRpcRequest = JsonRpcRequest("2.0", "eth_protocolVersion", None, Some(1))
    val ethResponse = jsonRpcController.handleRequest(ethRpcRequest).futureValue

    ethResponse.error shouldBe Some(JsonRpcErrors.MethodNotFound)
    ethResponse.result shouldBe None

    val web3RpcRequest = JsonRpcRequest("2.0", "web3_clientVersion", None, Some(1))
    val web3Response = jsonRpcController.handleRequest(web3RpcRequest).futureValue

    web3Response.error shouldBe None
    web3Response.result shouldBe Some(JString("etc-client/v0.1"))
  }

  it should "handle eth_getBlockTransactionCountByHash request" in new TestSetup {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    blockchain.save(blockToRequest)

    val rpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getBlockTransactionCountByHash",
      Some(JArray(List(JString(s"0x${blockToRequest.header.hashAsHexString}")))),
      Some(JInt(1))
    )
    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), Duration.Inf)

    val expectedTxCount = Extraction.decompose(BigInt(blockToRequest.body.transactionList.size))

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxCount)
  }

  it should "handle eth_getBlockByHash request" in new TestSetup {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockTd = blockToRequest.header.difficulty

    blockchain.save(blockToRequest)
    blockchain.save(blockToRequest.header.hash, blockTd)

    val request = JsonRpcRequest(
      "2.0",
      "eth_getBlockByHash",
      Some(JArray(List(JString(s"0x${blockToRequest.header.hashAsHexString}"), JBool(false)))),
      Some(JInt(1))
    )
    val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)

    val expectedBlockResponse = Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedBlockResponse)
  }

  it should "handle eth_getUncleByBlockHashAndIndex request" in new TestSetup {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchain.save(blockToRequest)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getUncleByBlockHashAndIndex",
      Some(JArray(List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)

    val expectedUncleBlockResponse = Extraction.decompose(BlockResponse(uncle, None))
      .removeField {
        case ("transactions", _) => true
        case _ => false
      }

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedUncleBlockResponse)
  }

  it should "handle eth_getTransactionByBlockHashAndIndex request" in new TestSetup {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2

    blockchain.save(blockToRequest)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionByBlockHashAndIndex",
      Some(JArray(List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(txIndexToRequest).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)
    val expectedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    )

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxResponse)
  }

  trait TestSetup extends MockFactory {
    def config: JsonRpcConfig = Config.Network.Rpc

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)

    val appStateStorage = mock[AppStateStorage]
    val web3Service = new Web3Service
    val ethService = new EthService(blockchain, appStateStorage)
    val netService = mock[NetService]
    val jsonRpcController = new JsonRpcController(web3Service, netService, ethService, config)
  }

}
