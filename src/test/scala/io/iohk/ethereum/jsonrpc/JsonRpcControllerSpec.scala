package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.{DefaultPatience, Fixtures}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.json4s.{DefaultFormats, Extraction, Formats}
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{BloomFilter, Ledger}
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.validators.Validators
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class JsonRpcControllerSpec extends FlatSpec with Matchers with ScalaFutures with DefaultPatience {

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
    response.error shouldBe Some(JsonRpcErrors.InvalidParams("Data 'asdasd' should have 0x prefix"))
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
    (netService.peerCount _).expects(*).returning(Future.successful(Right(PeerCountResponse(123))))

    val rpcRequest = JsonRpcRequest("2.0", "net_peerCount", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.result shouldBe Some(JString("0x7b"))
  }

  it should "Handle net_listening request" in new TestSetup {
    (netService.listening _).expects(*).returning(Future.successful(Right(ListeningResponse(false))))

    val rpcRequest = JsonRpcRequest("2.0", "net_listening", None, Some(1))

    val response = Await.result(jsonRpcController.handleRequest(rpcRequest), 3.seconds)

    response.result shouldBe Some(JBool(false))
  }

  it should "Handle net_version request" in new TestSetup {
    (netService.version _).expects(*).returning(Future.successful(Right(VersionResponse("99"))))

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

    val response = jsonRpcController.handleRequest(request).futureValue
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
        JString(s"0x500"),
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
    val appStateStorage = mock[AppStateStorage]
    val web3Service = new Web3Service
    val netService = mock[NetService]
    val personalService = mock[PersonalService]
    val ethService = new EthService(storagesInstance.storages, blockGenerator, appStateStorage, ledger,
      blockchainConfig, keyStore, pendingTransactionsManager.ref, syncingController.ref)
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
  }

}
