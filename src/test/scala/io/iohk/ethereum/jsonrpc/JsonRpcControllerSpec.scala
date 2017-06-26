package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, NormalPatience, Timeouts}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.FilterManager.{TxLog, LogFilterLogs}
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.{BlockchainConfig, Config, FilterConfig, MiningConfig}
import org.json4s.{DefaultFormats, Extraction, Formats}
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{BloomFilter, Ledger}
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.ommers.OmmersPool.Ommers
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.validators.Validators
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.duration._

// scalastyle:off file.size.limit
class JsonRpcControllerSpec extends FlatSpec with Matchers with PropertyChecks with ScalaFutures with NormalPatience with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  "JsonRpcController" should "handle valid sha3 request" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_sha3", Some(JArray(JString("0x1234") :: Nil)), Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x56570de287d73cd1cb6092bb8fdee6173974955fdef345ae579ee9f475ea7432"))
  }

  it should "fail when invalid request is received" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_sha3", Some(JArray(JString("asdasd") :: Nil)), Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe Some(JsonRpcErrors.InvalidParams("Invalid method parameters"))
  }

  it should "handle clientVersion request" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "web3_clientVersion", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("etc-client/v0.1"))
  }

  it should "Handle net_peerCount request" in new TestSetup {
    (netService.peerCount _).expects(*).returning(Future.successful(Right(PeerCountResponse(123))))

    val rpcRequest = JsonRpcRequest("2.0", "net_peerCount", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.result shouldBe Some(JString("0x7b"))
  }

  it should "Handle net_listening request" in new TestSetup {
    (netService.listening _).expects(*).returning(Future.successful(Right(ListeningResponse(false))))

    val rpcRequest = JsonRpcRequest("2.0", "net_listening", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.result shouldBe Some(JBool(false))
  }

  it should "Handle net_version request" in new TestSetup {
    (netService.version _).expects(*).returning(Future.successful(Right(VersionResponse("99"))))

    val rpcRequest = JsonRpcRequest("2.0", "net_version", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.result shouldBe Some(JString("99"))
  }

  it should "eth_protocolVersion" in new TestSetup {
    val rpcRequest = JsonRpcRequest("2.0", "eth_protocolVersion", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x3f"))
  }

  it should "handle eth_blockNumber request" in new TestSetup {
    val bestBlockNumber = 10
  (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val rpcRequest = JsonRpcRequest("2.0", "eth_blockNumber", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

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

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

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
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

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
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedBlockResponse = Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedBlockResponse)
  }

  it should "handle eth_getBlockByNumber request" in new TestSetup {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockTd = blockToRequest.header.difficulty

    blockchain.save(blockToRequest)
    blockchain.save(blockToRequest.header.hash, blockTd)

    val request = JsonRpcRequest(
      "2.0",
      "eth_getBlockByNumber",
      Some(JArray(List(JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"), JBool(false)))),
      Some(JInt(1))
    )
    val response = jsonRpcController.handleRequest(request).futureValue

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
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedUncleBlockResponse = Extraction.decompose(BlockResponse(uncle, None, pendingBlock = false))
      .removeField {
        case ("transactions", _) => true
        case _ => false
      }

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedUncleBlockResponse)
  }

  it should "handle eth_getUncleByBlockNumberAndIndex request" in new TestSetup {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchain.save(blockToRequest)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getUncleByBlockNumberAndIndex",
      Some(JArray(List(
        JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedUncleBlockResponse = Extraction.decompose(BlockResponse(uncle, None, pendingBlock = false))
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
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    )

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxResponse)
  }

  it should "personal_importRawKey" in new TestSetup {
    val key = "7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"
    val keyBytes = ByteString(Hex.decode(key))
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.importRawKey _).expects(ImportRawKeyRequest(keyBytes, pass))
      .returning(Future.successful(Right(ImportRawKeyResponse(addr))))

    val params = JArray(JString(key) :: JString(pass) :: Nil)
    val rpcRequest = JsonRpcRequest("2.0", "personal_importRawKey", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString(addr.toString))
  }

  it should "personal_newAccount" in new TestSetup {
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.newAccount _).expects(NewAccountRequest(pass))
      .returning(Future.successful(Right(NewAccountResponse(addr))))

    val params = JArray(JString(pass) :: Nil)
    val rpcRequest = JsonRpcRequest("2.0", "personal_newAccount", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString(addr.toString))
  }

  it should "personal_listAccounts" in new TestSetup {
    val addresses = List(34, 12391, 123).map(Address(_))
    val pass = "aaa"

    (personalService.listAccounts _).expects(ListAccountsRequest())
      .returning(Future.successful(Right(ListAccountsResponse(addresses))))

    val rpcRequest = JsonRpcRequest("2.0", "personal_listAccounts", None, Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(addresses.map(a => JString(a.toString))))
  }

  it should "personal_unlockAccount" in new TestSetup {
    val address = Address(42)
    val pass = "aaa"
    val params = JArray(JString(address.toString) :: JString(pass) :: Nil)

    (personalService.unlockAccount _).expects(UnlockAccountRequest(address, pass))
      .returning(Future.successful(Right(UnlockAccountResponse(true))))

    val rpcRequest = JsonRpcRequest("2.0", "personal_unlockAccount", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JBool(true))
  }

  it should "personal_lockAccount" in new TestSetup {
    val address = Address(42)
    val params = JArray(JString(address.toString) :: Nil)

    (personalService.lockAccount _).expects(LockAccountRequest(address))
      .returning(Future.successful(Right(LockAccountResponse(true))))

    val rpcRequest = JsonRpcRequest("2.0", "personal_lockAccount", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JBool(true))
  }

  it should "personal_sendTransaction" in new TestSetup {
    val params = JArray(
      JObject(
        "from" -> Address(42).toString,
        "to" -> Address(123).toString,
        "value" -> 1000
      ) :: JString("passphrase") :: Nil
    )

    val txHash = ByteString(1, 2, 3, 4)

    (personalService.sendTransaction(_: SendTransactionWithPassphraseRequest)).expects(*)
      .returning(Future.successful(Right(SendTransactionWithPassphraseResponse(txHash))))

    val rpcRequest = JsonRpcRequest("2.0", "personal_sendTransaction", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "eth_sendTransaction" in new TestSetup {
    val params = JArray(
      JObject(
        "from" -> Address(42).toString,
        "to" -> Address(123).toString,
        "value" -> 1000
      ) :: Nil
    )

    val txHash = ByteString(1, 2, 3, 4)

    (personalService.sendTransaction(_: SendTransactionRequest)).expects(*)
      .returning(Future.successful(Right(SendTransactionResponse(txHash))))

    val rpcRequest = JsonRpcRequest("2.0", "eth_sendTransaction", Some(params), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "eth_getWork" in new TestSetup {
    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    (appStateStorage.getBestBlockNumber _).expects().returns(1)
    (blockGenerator.generateBlockForMining _).expects(*, *, *, *)
      .returns(Right(Block(blockHeader, BlockBody(Nil, Nil))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getWork",
      None,
      Some(JInt(1))
    )

    val result: Future[JsonRpcResponse] = jsonRpcController.handleRequest(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(2))
    ommersPool.reply(Ommers(Nil))

    val response = result.futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(List(
      JString(headerPowHash),
      JString(seed),
      JString(target)
    )))
  }

  it should "eth_getWork when fail to get ommers and transactions" in new TestSetup {
    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    (appStateStorage.getBestBlockNumber _).expects().returns(1)
    (blockGenerator.generateBlockForMining _).expects(*, *, *, *)
      .returns(Right(Block(blockHeader, BlockBody(Nil, Nil))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getWork",
      None,
      Some(JInt(1))
    )

    val result: Future[JsonRpcResponse] = jsonRpcController.handleRequest(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    ommersPool.expectMsg(OmmersPool.GetOmmers(2))
    //on time out it should respond with empty list

    val response = result.futureValue(timeout(Timeouts.longTimeout))
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(List(
      JString(headerPowHash),
      JString(seed),
      JString(target)
    )))
  }

  it should "eth_submitWork" in new TestSetup {
    val nonce = s"0x0000000000000001"
    val mixHash =s"""0x${"01" * 32}"""
    val headerPowHash = "02" * 32

    (blockGenerator.getPrepared _)
      .expects(ByteString(Hex.decode(headerPowHash)))
      .returns(Some(Block(blockHeader, BlockBody(Nil, Nil))))
    (appStateStorage.getBestBlockNumber _).expects().returns(1)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_submitWork",
      Some(JArray(List(
        JString(nonce),
        JString(s"0x$headerPowHash"),
        JString(mixHash)
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JBool(true))
  }

  it should "eth_submitHashrate" in new TestSetup {
    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_submitHashrate",
      Some(JArray(List(
        JString(s"0x${"0" * 61}500"),
        JString(s"0x59daa26581d0acd1fce254fb7e85952f4c09d0915afd33d3886cd914bc7d283c")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JBool(true))
  }

  it should "eth_hashrate" in new TestSetup {
    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_hashrate",
      None,
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x0"))
  }

  it should "eth_gasPrice" in new TestSetup {
    (appStateStorage.getBestBlockNumber _).expects().returning(42)
    blockchain.save(Block(Fixtures.Blocks.Block3125369.header.copy(number = 42), Fixtures.Blocks.Block3125369.body))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_gasPrice",
      None,
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x4a817c800"))
  }

  it should "eth_call" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.call _).expects(*).returning(Future.successful(Right(CallResponse(ByteString("asd")))))

    val json = JArray(List(
      JObject(
        "from" -> "0xabbb6bebfa05aa13e908eaa492bd7a8343760477",
        "to" -> "0xda714fe079751fa7a1ad80b76571ea6ec52a446c",
        "gas" -> "0x12",
        "gasPrice" -> "0x123",
        "value" -> "0x99",
        "data" -> "0xFF44"
      ),
      JString("latest")
    ))
    val rpcRequest = JsonRpcRequest("2.0", "eth_call", Some(json), Some(1))
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x617364"))
  }

  it should "eth_estimateGas" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.estimateGas _).expects(*).anyNumberOfTimes().returning(Future.successful(Right(EstimateGasResponse(2310))))

    val callObj = JObject(
      "from" -> "0xabbb6bebfa05aa13e908eaa492bd7a8343760477",
      "to" -> "0xda714fe079751fa7a1ad80b76571ea6ec52a446c",
      "gas" -> "0x12",
      "gasPrice" -> "0x123",
      "value" -> "0x99",
      "data" -> "0xFF44"
    )
    val callObjWithoutData = callObj.replace(List("data"), "")

    val table = Table(
      "Requests",
      JArray(List(callObj, JString("latest"))),
      JArray(List(callObj)),
      JArray(List(callObjWithoutData))
    )

    forAll(table) { json =>
      val rpcRequest = JsonRpcRequest("2.0", "eth_estimateGas", Some(json), Some(1))
      val response = jsonRpcController.handleRequest(rpcRequest).futureValue

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JString("0x906"))
    }

  }

  it should "eth_getCode" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getCode _).expects(*).returning(Future.successful(Right(GetCodeResponse(ByteString(Hex.decode("FFAA22"))))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getCode",
      Some(JArray(List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0xffaa22"))
  }

  it should "eth_getUncleCountByBlockNumber" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getUncleCountByBlockNumber _).expects(*)
      .returning(Future.successful(Right(GetUncleCountByBlockNumberResponse(2))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getUncleCountByBlockNumber",
      Some(JArray(List(
        JString(s"0x12")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x2"))
  }

  it should "eth_getUncleCountByBlockHash " in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getUncleCountByBlockHash _).expects(*)
      .returning(Future.successful(Right(GetUncleCountByBlockHashResponse(3))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getUncleCountByBlockHash",
      Some(JArray(List(
        JString(s"0x7dc64cb9d8a95763e288d71088fe3116e10dbff317c09f7a9bd5dd6974d27d20")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x3"))
  }

  it should "eth_getBlockTransactionCountByNumber " in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getBlockTransactionCountByNumber _).expects(*)
      .returning(Future.successful(Right(GetBlockTransactionCountByNumberResponse(17))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getBlockTransactionCountByNumber",
      Some(JArray(List(
        JString(s"0x123")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x11"))
  }

  it should "eth_coinbase " in new TestSetup {
    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_coinbase",
      None,
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x" + "42" * 20))
  }

  it should "eth_getTransactionByBlockNumberAndIndex by tag" in new TestSetup {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returns(blockToRequest.header.number)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionByBlockNumberAndIndex",
      Some(JArray(List(
        JString(s"latest"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by hex number" in new TestSetup {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header.copy(number = BigInt(0xC005)), Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.save(blockToRequest)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionByBlockNumberAndIndex",
      Some(JArray(List(
        JString(s"0xC005"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by number" in new TestSetup {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.save(blockToRequest)

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionByBlockNumberAndIndex",
      Some(JArray(List(
        JInt(Fixtures.Blocks.Block3125369.header.number),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      ))),
      Some(JInt(1))
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(expectedTxResponse)
  }

  it should "eth_getBalance" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getBalance _).expects(*)
      .returning(Future.successful(Right(GetBalanceResponse(17))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getBalance",
      Some(JArray(List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x11"))
  }

  it should "eth_getStorageAt" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getStorageAt _).expects(*)
      .returning(Future.successful(Right(GetStorageAtResponse(ByteString("response")))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getStorageAt",
      Some(JArray(List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"0x01"),
        JString(s"latest")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x" + Hex.toHexString(ByteString("response").toArray[Byte])))
  }

  it should "eth_getTransactionCount" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getTransactionCount _).expects(*)
      .returning(Future.successful(Right(GetTransactionCountResponse(123))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionCount",
      Some(JArray(List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x7b"))
  }

  it should "eth_getTransactionByHash" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    val txResponse = TransactionResponse(Fixtures.Blocks.Block3125369.body.transactionList.head)
    (mockEthService.getTransactionByHash _).expects(*)
      .returning(Future.successful(Right(GetTransactionByHashResponse(Some(txResponse)))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionByHash",
      Some(JArray(List(
        JString("0xe9b2d3e8a2bc996a1c7742de825fdae2466ae783ce53484304efffe304ff232d")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(Extraction.decompose(txResponse))
  }

  it should "eth_sign" in new TestSetup {

    (personalService.sign _).expects(
      SignRequest(
        ByteString(Hex.decode("deadbeaf")),
        Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83"))),
        None))
      .returns(Future.successful(Right(SignResponse(sig))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_sign",
      Some(JArray(List(
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString(s"0xdeadbeaf")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"))
  }

  it should "personal_sign" in new TestSetup {

    (personalService.sign _).expects(
        SignRequest(
          ByteString(Hex.decode("deadbeaf")),
          Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83"))),
          Some("thePassphrase")))
      .returns(Future.successful(Right(SignResponse(sig))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "personal_sign",
      Some(JArray(List(
        JString(s"0xdeadbeaf"),
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString("thePassphrase")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"))
  }

  it should "personal_ecRecover" in new TestSetup {

    (personalService.ecRecover _).expects(EcRecoverRequest(ByteString(Hex.decode("deadbeaf")), sig))
      .returns(Future.successful(Right(EcRecoverResponse(Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83")))))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "personal_ecRecover",
      Some(JArray(List(
        JString(s"0xdeadbeaf"),
        JString(s"0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b")
      ))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"))
  }

  it should "eth_newFilter" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.newFilter _).expects(*)
      .returning(Future.successful(Right(NewFilterResponse(123))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_newFilter",
      Some(JArray(List(JObject(
        "fromBlock" -> "0x0",
        "toBlock" -> "latest",
        "address" -> "0x2B5A350698C91E684EB08c10F7e462f761C0e681",
        "topics" -> JArray(List(JNull, "0x00000000000000000000000000000000000000000000000000000000000001c8"))
      )))),
      Some(JInt(1))
    )



    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JString("0x7b"))
  }

  it should "eth_newBlockFilter" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.newBlockFilter _).expects(*)
      .returning(Future.successful(Right(NewFilterResponse(999))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_newBlockFilter",
      Some(JArray(List())),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.result shouldBe Some(JString("0x3e7"))
  }

  it should "eth_newPendingTransactionFilter" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.newPendingTransactionFilter _).expects(*)
      .returning(Future.successful(Right(NewFilterResponse(2))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_newPendingTransactionFilter",
      Some(JArray(List())),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.result shouldBe Some(JString("0x2"))
  }

  it should "eth_uninstallFilter" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.uninstallFilter _).expects(*)
      .returning(Future.successful(Right(UninstallFilterResponse(true))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_uninstallFilter",
      Some(JArray(List(JString("0x1")))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JBool(true))
  }

  it should "eth_getFilterChanges" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getFilterChanges _).expects(*)
      .returning(Future.successful(Right(GetFilterChangesResponse(FilterManager.LogFilterChanges(Seq(
        FilterManager.TxLog(
        logIndex = 0,
        transactionIndex = 0,
        transactionHash = ByteString(Hex.decode("123ffa")),
        blockHash = ByteString(Hex.decode("123eeaa22a")),
        blockNumber = 99,
        address = Address("0x123456"),
        data = ByteString(Hex.decode("ff33")),
        topics = Seq(ByteString(Hex.decode("33")), ByteString(Hex.decode("55"))))))))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getFilterChanges",
      Some(JArray(List(JString("0x1")))),
      Some(JInt(1)))

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(List(JObject(
      "logIndex" -> JString("0x0"),
      "transactionIndex" -> JString("0x0"),
      "transactionHash" -> JString("0x123ffa"),
      "blockHash" -> JString("0x123eeaa22a"),
      "blockNumber" -> JString("0x63"),
      "address" -> JString("0x0000000000000000000000000000000000123456"),
      "data" -> JString("0xff33"),
      "topics" -> JArray(List(JString("0x33"), JString("0x55")))))))
  }

  it should "eth_getFilterLogs" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getFilterLogs _).expects(*)
      .returning(Future.successful(Right(GetFilterLogsResponse(FilterManager.BlockFilterLogs(Seq(
        ByteString(Hex.decode("1234")),
        ByteString(Hex.decode("4567")),
        ByteString(Hex.decode("7890"))
      ))))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getFilterLogs",
      Some(JArray(List(JString("0x1")))),
      Some(JInt(1)))

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(List(
      JString("0x1234"),
      JString("0x4567"),
      JString("0x7890"))))
  }

  it should "eth_getLogs" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    (mockEthService.getLogs _).expects(*)
      .returning(Future.successful(Right(GetLogsResponse(LogFilterLogs(Seq(
        FilterManager.TxLog(
          logIndex = 0,
          transactionIndex = 0,
          transactionHash = ByteString(Hex.decode("123ffa")),
          blockHash = ByteString(Hex.decode("123eeaa22a")),
          blockNumber = 99,
          address = Address("0x123456"),
          data = ByteString(Hex.decode("ff33")),
          topics = Seq(ByteString(Hex.decode("33")), ByteString(Hex.decode("55"))))))))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getLogs",
      Some(JArray(List(JObject(
        "fromBlock" -> "0x0",
        "toBlock" -> "latest",
        "address" -> "0x2B5A350698C91E684EB08c10F7e462f761C0e681",
        "topics" -> JArray(List(JNull, "0x00000000000000000000000000000000000000000000000000000000000001c8"))
      )))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JArray(List(JObject(
      "logIndex" -> JString("0x0"),
      "transactionIndex" -> JString("0x0"),
      "transactionHash" -> JString("0x123ffa"),
      "blockHash" -> JString("0x123eeaa22a"),
      "blockNumber" -> JString("0x63"),
      "address" -> JString("0x0000000000000000000000000000000000123456"),
      "data" -> JString("0xff33"),
      "topics" -> JArray(List(JString("0x33"), JString("0x55")))))))
  }

 it should "rpc_modules" in new TestSetup {
    val request: JsonRpcRequest = JsonRpcRequest("2.0", "rpc_modules", None, Some(JInt(1)))

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JObject(
      "net" -> "1.0",
      "rpc" -> "1.0",
      "personal" -> "1.0",
      "eth" -> "1.0",
      "web3" -> "1.0"
    ))
  }

  it should "eth_getTransactionReceipt" in new TestSetup {
    val mockEthService = mock[EthService]
    override val jsonRpcController = new JsonRpcController(web3Service, netService, mockEthService, personalService, config)

    val arbitraryValue = 42

    val mockRequest = GetTransactionReceiptRequest(ByteString(Hex.decode("b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238")))

    val mockResponse = Right(GetTransactionReceiptResponse(Some(
      TransactionReceiptResponse(
        transactionHash = ByteString(Hex.decode("23" * 32)),
        transactionIndex = 1,
        blockNumber = Fixtures.Blocks.Block3125369.header.number,
        blockHash = Fixtures.Blocks.Block3125369.header.hash,
        cumulativeGasUsed = arbitraryValue * 10,
        gasUsed = arbitraryValue,
        contractAddress = Some(Address(arbitraryValue)),
        logs = Seq(TxLog(
          logIndex = 0,
          transactionIndex = 1,
          transactionHash = ByteString(Hex.decode("23" * 32)),
          blockHash = Fixtures.Blocks.Block3125369.header.hash,
          blockNumber = Fixtures.Blocks.Block3125369.header.number,
          address = Address(arbitraryValue),
          data = ByteString(Hex.decode("43" * 32)),
          topics = Seq(ByteString(Hex.decode("44" * 32)), ByteString(Hex.decode("45" * 32)))))))))

    (mockEthService.getTransactionReceipt _).expects(*).returning(Future.successful(mockResponse))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getTransactionReceipt",
      Some(JArray(List(JString(s"0xb903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238")))),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response.jsonrpc shouldBe "2.0"
    response.id shouldBe JInt(1)
    response.error shouldBe None
    response.result shouldBe Some(JObject(
      JField("transactionHash", JString("0x" + "23" * 32)),
      JField("transactionIndex", JString("0x1")),
      JField("blockNumber", JString("0x2fb079")),
      JField("blockHash", JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))),
      JField("cumulativeGasUsed", JString("0x1a4")),
      JField("gasUsed", JString("0x2a")),
      JField("contractAddress", JString("0x000000000000000000000000000000000000002a")),
      JField("logs", JArray(List(JObject(
        JField("logIndex", JString("0x0")),
        JField("transactionIndex", JString("0x1")),
        JField("transactionHash", JString("0x" + "23" * 32)),
        JField("blockHash", JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))),
        JField("blockNumber", JString("0x2fb079")),
        JField("address", JString("0x000000000000000000000000000000000000002a")),
        JField("data", JString("0x" + "43" * 32)),
        JField("topics", JArray(List(JString("0x" + "44" * 32), JString("0x" + "45" * 32))))))))
    ))
  }

  trait TestSetup extends MockFactory {
    def config: JsonRpcConfig = Config.Network.Rpc

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)
    val blockGenerator: BlockGenerator = mock[BlockGenerator]
    implicit val system = ActorSystem("JsonRpcControllerSpec_System")

    val syncingController = TestProbe()
    val ledger = mock[Ledger]
    val validators = mock[Validators]
    val blockchainConfig = mock[BlockchainConfig]
    val keyStore = mock[KeyStore]

    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()
    val filterManager = TestProbe()

    val miningConfig = new MiningConfig {
      override val coinbase: Address = Address(Hex.decode("42" * 20))
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val txPoolSize: Int = 30
      override val poolingServicesTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val filterConfig = new FilterConfig {
      override val filterTimeout: FiniteDuration = 3.seconds
      override val filterManagerQueryTimeout: FiniteDuration = 3.seconds
      override val pendingTransactionsManagerQueryTimeout: FiniteDuration = 3.seconds
    }

    val appStateStorage = mock[AppStateStorage]
    val web3Service = new Web3Service
    val netService = mock[NetService]
    val personalService = mock[PersonalService]
    val ethService = new EthService(storagesInstance.storages, blockGenerator, appStateStorage, miningConfig, ledger,
      keyStore, pendingTransactionsManager.ref, syncingController.ref, ommersPool.ref, filterManager.ref, filterConfig)
    val jsonRpcController = new JsonRpcController(web3Service, netService, ethService, personalService, config)

    val blockHeader = BlockHeader(
      parentHash = ByteString("unused"),
      ommersHash = ByteString("unused"),
      beneficiary = ByteString("unused"),
      stateRoot = ByteString("unused"),
      transactionsRoot = ByteString("unused"),
      receiptsRoot = ByteString("unused"),
      logsBloom = BloomFilter.EmptyBloomFilter,
      difficulty = 10,
      number = 2,
      gasLimit = 0,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused"))

    val r: ByteString = ByteString(Hex.decode("a3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a1"))
    val s: ByteString = ByteString(Hex.decode("2d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee"))
    val v: ByteString = ByteString(Hex.decode("1b"))
    val sig = ECDSASignature(r, s, v)
  }

}
