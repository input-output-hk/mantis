package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import org.bouncycastle.util.encoders.Hex
import org.json4s.DefaultFormats
import org.json4s.Extraction
import org.json4s.Formats
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.LongPatience
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthBlocksService.GetUncleCountByBlockHashResponse
import io.iohk.ethereum.jsonrpc.EthBlocksService.GetUncleCountByBlockNumberResponse
import io.iohk.ethereum.jsonrpc.EthFilterService._
import io.iohk.ethereum.jsonrpc.EthInfoService._
import io.iohk.ethereum.jsonrpc.EthUserService._
import io.iohk.ethereum.jsonrpc.FilterManager.LogFilterLogs
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.ProofService.GetProofRequest
import io.iohk.ethereum.jsonrpc.ProofService.GetProofResponse
import io.iohk.ethereum.jsonrpc.ProofService.ProofAccount
import io.iohk.ethereum.jsonrpc.ProofService.StorageProofKey
import io.iohk.ethereum.jsonrpc.ProofService.StorageValueProof
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.OptionNoneToJNullSerializer
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.QuantitiesSerializer
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.UnformattedDataJsonSerializer
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.ommers.OmmersPool.Ommers
import io.iohk.ethereum.testing.ActorsTesting.simpleAutoPilot
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.BlockchainConfig

// scalastyle:off magic.number
class JsonRpcControllerEthSpec
    extends TestKit(ActorSystem("JsonRpcControllerEthSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  it should "eth_protocolVersion" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("eth_protocolVersion")
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult("0x3f")
  }

  it should "handle eth_chainId" in new JsonRpcControllerFixture {
    val request = newJsonRpcRequest("eth_chainId")
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    response should haveStringResult("0x3d")
  }

  it should "handle eth_blockNumber request" in new JsonRpcControllerFixture {
    val bestBlockNumber = 10
    blockchain.saveBestKnownBlocks(ByteString.empty, bestBlockNumber)

    val rpcRequest = newJsonRpcRequest("eth_blockNumber")
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult(s"0xa")
  }

  it should "eth_syncing" in new JsonRpcControllerFixture {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.Syncing(999, Progress(200, 10000), Some(Progress(100, 144)))
    })

    val rpcRequest = JsonRpcRequest("2.0", "eth_syncing", None, Some(1))

    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveObjectResult(
      "startingBlock" -> "0x3e7",
      "currentBlock" -> "0xc8",
      "highestBlock" -> "0x2710",
      "knownStates" -> "0x90",
      "pulledStates" -> "0x64"
    )
  }

  it should "handle eth_getBlockByHash request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()

    val request = newJsonRpcRequest(
      "eth_getBlockByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByHash request (block with checkpoint)" in new JsonRpcControllerFixture {
    val blockToRequest = blockWithCheckpoint
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()

    val request = newJsonRpcRequest(
      "eth_getBlockByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByHash request (block with treasuryOptOut)" in new JsonRpcControllerFixture {
    val blockToRequest = blockWithTreasuryOptOut
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()

    val request = newJsonRpcRequest(
      "eth_getBlockByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByNumber request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()
    blockchain.saveBestKnownBlocks(blockToRequest.hash, blockToRequest.number)

    val request = newJsonRpcRequest(
      "eth_getBlockByNumber",
      List(JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByNumber request (block with treasuryOptOut)" in new JsonRpcControllerFixture {
    val blockToRequest = blockWithTreasuryOptOut
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()
    blockchain.saveBestKnownBlocks(blockToRequest.hash, blockToRequest.number)

    val request = newJsonRpcRequest(
      "eth_getBlockByNumber",
      List(JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByNumber request (block with checkpoint)" in new JsonRpcControllerFixture {
    val blockToRequest = blockWithCheckpoint
    val blockWeight = ChainWeight.zero.increase(blockToRequest.header)

    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequest.header.hash, blockWeight))
      .commit()
    blockchain.saveBestKnownBlocks(blockToRequest.hash, blockToRequest.number)

    val request = newJsonRpcRequest(
      "eth_getBlockByNumber",
      List(JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getUncleByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchainWriter.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedUncleBlockResponse = Extraction
      .decompose(BlockResponse(uncle, None, pendingBlock = false))
      .removeField {
        case ("transactions", _) => true
        case _                   => false
      }

    response should haveResult(expectedUncleBlockResponse)
  }

  it should "handle eth_getUncleByBlockNumberAndIndex request" in new JsonRpcControllerFixture {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchainWriter.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.hash, blockToRequest.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleByBlockNumberAndIndex",
      List(
        JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    val expectedUncleBlockResponse = Extraction
      .decompose(BlockResponse(uncle, None, pendingBlock = false))
      .removeField {
        case ("transactions", _) => true
        case _                   => false
      }

    response should haveResult(expectedUncleBlockResponse)
  }

  it should "eth_getWork" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    blockchainWriter.save(parentBlock, Nil, ChainWeight.zero.increase(parentBlock.header), true)
    (blockGenerator
      .generateBlock(
        _: Block,
        _: Seq[SignedTransaction],
        _: Address,
        _: Seq[BlockHeader],
        _: Option[InMemoryWorldStateProxy]
      )(_: BlockchainConfig))
      .expects(parentBlock, *, *, *, *, *)
      .returns(PendingBlockAndState(PendingBlock(Block(blockHeader, BlockBody(Nil, Nil)), Nil), fakeWorld))

    val request: JsonRpcRequest = newJsonRpcRequest("eth_getWork")

    val response: JsonRpcResponse = jsonRpcController.handleRequest(request).runSyncUnsafe()

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    ommersPool.reply(Ommers(Nil))

    response should haveResult(
      JArray(
        List(
          JString(headerPowHash),
          JString(seed),
          JString(target)
        )
      )
    )
  }

  it should "eth_getWork when fail to get ommers and transactions" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    blockchainWriter.save(parentBlock, Nil, ChainWeight.zero.increase(parentBlock.header), true)
    (blockGenerator
      .generateBlock(
        _: Block,
        _: Seq[SignedTransaction],
        _: Address,
        _: Seq[BlockHeader],
        _: Option[InMemoryWorldStateProxy]
      )(_: BlockchainConfig))
      .expects(parentBlock, *, *, *, *, *)
      .returns(PendingBlockAndState(PendingBlock(Block(blockHeader, BlockBody(Nil, Nil)), Nil), fakeWorld))

    val request: JsonRpcRequest = newJsonRpcRequest("eth_getWork")

    val result: JsonRpcResponse = jsonRpcController
      .handleRequest(request)
      .timeout(Timeouts.longTimeout)
      .runSyncUnsafe()

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    //on time out it should respond with empty list

    val response = result
    response should haveResult(
      JArray(
        List(
          JString(headerPowHash),
          JString(seed),
          JString(target)
        )
      )
    )
  }

  it should "eth_submitWork" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val nonce = s"0x0000000000000001"
    val mixHash = s"""0x${"01" * 32}"""
    val headerPowHash = "02" * 32

    (blockGenerator.getPrepared _)
      .expects(ByteString(Hex.decode(headerPowHash)))
      .returns(Some(PendingBlock(Block(blockHeader, BlockBody(Nil, Nil)), Nil)))
    (appStateStorage.getBestBlockNumber _).expects().returns(1)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_submitWork",
      List(
        JString(nonce),
        JString(s"0x$headerPowHash"),
        JString(mixHash)
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveBooleanResult(true)
  }

  it should "eth_submitHashrate" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_submitHashrate",
      List(
        JString(s"0x${"0" * 61}500"),
        JString(s"0x59daa26581d0acd1fce254fb7e85952f4c09d0915afd33d3886cd914bc7d283c")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveBooleanResult(true)
  }

  it should "eth_hashrate" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val request: JsonRpcRequest = newJsonRpcRequest("eth_hashrate")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x0")
  }

  it should "eth_gasPrice" in new JsonRpcControllerFixture {
    blockchainWriter
      .storeBlock(Block(Fixtures.Blocks.Block3125369.header.copy(number = 42), Fixtures.Blocks.Block3125369.body))
      .commit()
    blockchain.saveBestKnownBlocks(ByteString.empty, 42)

    val request: JsonRpcRequest = newJsonRpcRequest("eth_gasPrice")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x4a817c800")
  }

  it should "eth_call" in new JsonRpcControllerFixture {
    val mockEthInfoService = mock[EthInfoService]
    override val jsonRpcController = super.jsonRpcController.copy(ethInfoService = mockEthInfoService)

    (mockEthInfoService.call _).expects(*).returning(Task.now(Right(CallResponse(ByteString("asd")))))

    val json = List(
      JObject(
        "from" -> "0xabbb6bebfa05aa13e908eaa492bd7a8343760477",
        "to" -> "0xda714fe079751fa7a1ad80b76571ea6ec52a446c",
        "gas" -> "0x12",
        "gasPrice" -> "0x123",
        "value" -> "0x99",
        "data" -> "0xFF44"
      ),
      JString("latest")
    )
    val rpcRequest = newJsonRpcRequest("eth_call", json)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult("0x617364")
  }

  it should "eth_estimateGas" in new JsonRpcControllerFixture {
    val mockEthInfoService = mock[EthInfoService]
    override val jsonRpcController = super.jsonRpcController.copy(ethInfoService = mockEthInfoService)

    (mockEthInfoService.estimateGas _)
      .expects(*)
      .anyNumberOfTimes()
      .returning(Task.now(Right(EstimateGasResponse(2310))))

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
      List(callObj, JString("latest")),
      List(callObj),
      List(callObjWithoutData)
    )

    forAll(table) { json =>
      val rpcRequest = newJsonRpcRequest("eth_estimateGas", json)
      val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

      response should haveStringResult("0x906")
    }

  }

  it should "eth_getCode" in new JsonRpcControllerFixture {
    val mockEthUserService = mock[EthUserService]
    override val jsonRpcController = super.jsonRpcController.copy(ethUserService = mockEthUserService)

    (mockEthUserService.getCode _)
      .expects(*)
      .returning(Task.now(Right(GetCodeResponse(ByteString(Hex.decode("FFAA22"))))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getCode",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0xffaa22")
  }

  it should "eth_getUncleCountByBlockNumber" in new JsonRpcControllerFixture {
    val mockEthBlocksService = mock[EthBlocksService]
    override val jsonRpcController = super.jsonRpcController.copy(ethBlocksService = mockEthBlocksService)

    (mockEthBlocksService.getUncleCountByBlockNumber _)
      .expects(*)
      .returning(Task.now(Right(GetUncleCountByBlockNumberResponse(2))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleCountByBlockNumber",
      List(
        JString(s"0x12")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x2")
  }

  it should "eth_getUncleCountByBlockHash " in new JsonRpcControllerFixture {
    val mockEthBlocksService = mock[EthBlocksService]
    override val jsonRpcController = super.jsonRpcController.copy(ethBlocksService = mockEthBlocksService)

    (mockEthBlocksService.getUncleCountByBlockHash _)
      .expects(*)
      .returning(Task.now(Right(GetUncleCountByBlockHashResponse(3))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleCountByBlockHash",
      List(
        JString(s"0x7dc64cb9d8a95763e288d71088fe3116e10dbff317c09f7a9bd5dd6974d27d20")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x3")
  }

  it should "eth_coinbase " in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    val request: JsonRpcRequest = newJsonRpcRequest("eth_coinbase")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x000000000000000000000000000000000000002a")
  }

  it should "eth_getBalance" in new JsonRpcControllerFixture {
    val mockEthUserService = mock[EthUserService]
    override val jsonRpcController = super.jsonRpcController.copy(ethUserService = mockEthUserService)

    (mockEthUserService.getBalance _)
      .expects(*)
      .returning(Task.now(Right(GetBalanceResponse(17))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getBalance",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x11")
  }

  it should "return error with custom error in data in eth_balance" in new JsonRpcControllerFixture {
    val mockEthUserService = mock[EthUserService]
    override val jsonRpcController = super.jsonRpcController.copy(ethUserService = mockEthUserService)

    (mockEthUserService.getBalance _)
      .expects(*)
      .returning(Task.now(Left(JsonRpcError.NodeNotFound)))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getBalance",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(JsonRpcError.NodeNotFound)
  }

  it should "eth_getStorageAt" in new JsonRpcControllerFixture {
    val mockEthUserService = mock[EthUserService]
    override val jsonRpcController = super.jsonRpcController.copy(ethUserService = mockEthUserService)

    (mockEthUserService.getStorageAt _)
      .expects(*)
      .returning(Task.now(Right(GetStorageAtResponse(ByteString("response")))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getStorageAt",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"0x01"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(JString("0x" + Hex.toHexString(ByteString("response").toArray[Byte])))
  }

  it should "eth_sign" in new JsonRpcControllerFixture {

    (personalService.sign _)
      .expects(
        SignRequest(
          ByteString(Hex.decode("deadbeaf")),
          Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83"))),
          None
        )
      )
      .returns(Task.now(Right(SignResponse(sig))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_sign",
      List(
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString(s"0xdeadbeaf")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult(
      "0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"
    )
  }

  it should "eth_newFilter" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.newFilter _)
      .expects(*)
      .returning(Task.now(Right(NewFilterResponse(123))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_newFilter",
      List(
        JObject(
          "fromBlock" -> "0x0",
          "toBlock" -> "latest",
          "address" -> "0x2B5A350698C91E684EB08c10F7e462f761C0e681",
          "topics" -> JArray(List(JNull, "0x00000000000000000000000000000000000000000000000000000000000001c8"))
        )
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x7b")
  }

  it should "eth_newBlockFilter" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.newBlockFilter _)
      .expects(*)
      .returning(Task.now(Right(NewFilterResponse(999))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_newBlockFilter",
      Some(JArray(List())),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x3e7")
  }

  it should "eth_newPendingTransactionFilter" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.newPendingTransactionFilter _)
      .expects(*)
      .returning(Task.now(Right(NewFilterResponse(2))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_newPendingTransactionFilter",
      Nil
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x2")
  }

  it should "eth_uninstallFilter" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.uninstallFilter _)
      .expects(*)
      .returning(Task.now(Right(UninstallFilterResponse(true))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_uninstallFilter",
      List(JString("0x1"))
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveBooleanResult(true)
  }

  it should "eth_getFilterChanges" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.getFilterChanges _)
      .expects(*)
      .returning(
        Task.now(
          Right(
            GetFilterChangesResponse(
              FilterManager.LogFilterChanges(
                Seq(
                  FilterManager.TxLog(
                    logIndex = 0,
                    transactionIndex = 0,
                    transactionHash = ByteString(Hex.decode("123ffa")),
                    blockHash = ByteString(Hex.decode("123eeaa22a")),
                    blockNumber = 99,
                    address = Address("0x123456"),
                    data = ByteString(Hex.decode("ff33")),
                    topics = Seq(ByteString(Hex.decode("33")), ByteString(Hex.decode("55")))
                  )
                )
              )
            )
          )
        )
      )

    val request: JsonRpcRequest =
      newJsonRpcRequest("eth_getFilterChanges", List(JString("0x1")))

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(
      JArray(
        List(
          JObject(
            "logIndex" -> JString("0x0"),
            "transactionIndex" -> JString("0x0"),
            "transactionHash" -> JString("0x123ffa"),
            "blockHash" -> JString("0x123eeaa22a"),
            "blockNumber" -> JString("0x63"),
            "address" -> JString("0x0000000000000000000000000000000000123456"),
            "data" -> JString("0xff33"),
            "topics" -> JArray(List(JString("0x33"), JString("0x55")))
          )
        )
      )
    )
  }

  it should "decode and encode eth_getProof request and response" in new JsonRpcControllerFixture {
    val address = "0x7F0d15C7FAae65896648C8273B6d7E43f58Fa842"

    val request: JsonRpcRequest = JsonRpcRequest(
      jsonrpc = "2.0",
      method = "eth_getProof",
      params = Some(
        JArray(
          List(
            JString(address),
            JArray(List(JString("0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))),
            JString("latest")
          )
        )
      ),
      id = Some(JInt(1))
    )

    val expectedDecodedRequest = GetProofRequest(
      address = Address(address),
      storageKeys =
        List(StorageProofKey(BigInt("39309028074332508661983559455579427211983204215636056653337583610388178777121"))),
      blockNumber = BlockParam.Latest
    )
    val expectedEncodedResponse: GetProofResponse = GetProofResponse(
      ProofAccount(
        address = Address(address),
        accountProof = Seq(ByteString(Hex.decode("1234"))),
        balance = BigInt(0x0),
        codeHash = ByteString(Hex.decode("123eeaa22a")),
        nonce = 0,
        storageHash = ByteString(Hex.decode("1a2b3c")),
        storageProof = Seq(
          StorageValueProof(
            key = StorageProofKey(42),
            value = BigInt(2000),
            proof = Seq(
              ByteString(Hex.decode("dead")),
              ByteString(Hex.decode("beef"))
            )
          )
        )
      )
    )

    // setup
    val mockEthProofService = mock[EthProofService]
    override val jsonRpcController = super.jsonRpcController.copy(proofService = mockEthProofService)
    (mockEthProofService.getProof _)
      .expects(expectedDecodedRequest)
      .returning(Task.now(Right(expectedEncodedResponse)))

    // when
    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()

    // then
    response should haveObjectResult(
      "accountProof" -> JArray(
        List(
          JString("0x1234")
        )
      ),
      "balance" -> JString("0x0"),
      "codeHash" -> JString("0x123eeaa22a"),
      "nonce" -> JString("0x0"),
      "storageHash" -> JString("0x1a2b3c"),
      "storageProof" -> JArray(
        List(
          JObject(
            "key" -> JString("0x2a"),
            "proof" -> JArray(
              List(
                JString("0xdead"),
                JString("0xbeef")
              )
            ),
            "value" -> JString("0x7d0")
          )
        )
      )
    )
  }

  it should "return error with custom error in data in eth_getProof" in new JsonRpcControllerFixture {
    val mockEthProofService = mock[EthProofService]
    override val jsonRpcController = super.jsonRpcController.copy(proofService = mockEthProofService)

    (mockEthProofService.getProof _)
      .expects(*)
      .returning(Task.now(Left(JsonRpcError.NodeNotFound)))

    val request: JsonRpcRequest =
      newJsonRpcRequest(
        "eth_getProof",
        List(
          JString("0x7F0d15C7FAae65896648C8273B6d7E43f58Fa842"),
          JArray(List(JString("0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))),
          JString("latest")
        )
      )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(JsonRpcError.NodeNotFound)
  }

  it should "eth_getFilterLogs" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.getFilterLogs _)
      .expects(*)
      .returning(
        Task.now(
          Right(
            GetFilterLogsResponse(
              FilterManager.BlockFilterLogs(
                Seq(
                  ByteString(Hex.decode("1234")),
                  ByteString(Hex.decode("4567")),
                  ByteString(Hex.decode("7890"))
                )
              )
            )
          )
        )
      )

    val request: JsonRpcRequest =
      newJsonRpcRequest("eth_getFilterLogs", List(JString("0x1")))

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(JArray(List(JString("0x1234"), JString("0x4567"), JString("0x7890"))))
  }

  it should "eth_getLogs" in new JsonRpcControllerFixture {
    val mockEthFilterService = mock[EthFilterService]
    override val jsonRpcController = super.jsonRpcController.copy(ethFilterService = mockEthFilterService)

    (mockEthFilterService.getLogs _)
      .expects(*)
      .returning(
        Task.now(
          Right(
            GetLogsResponse(
              LogFilterLogs(
                Seq(
                  FilterManager.TxLog(
                    logIndex = 0,
                    transactionIndex = 0,
                    transactionHash = ByteString(Hex.decode("123ffa")),
                    blockHash = ByteString(Hex.decode("123eeaa22a")),
                    blockNumber = 99,
                    address = Address("0x123456"),
                    data = ByteString(Hex.decode("ff33")),
                    topics = Seq(ByteString(Hex.decode("33")), ByteString(Hex.decode("55")))
                  )
                )
              )
            )
          )
        )
      )

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getLogs",
      List(
        JObject(
          "fromBlock" -> "0x0",
          "toBlock" -> "latest",
          "address" -> "0x2B5A350698C91E684EB08c10F7e462f761C0e681",
          "topics" -> JArray(List(JNull, "0x00000000000000000000000000000000000000000000000000000000000001c8"))
        )
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(
      JArray(
        List(
          JObject(
            "logIndex" -> JString("0x0"),
            "transactionIndex" -> JString("0x0"),
            "transactionHash" -> JString("0x123ffa"),
            "blockHash" -> JString("0x123eeaa22a"),
            "blockNumber" -> JString("0x63"),
            "address" -> JString("0x0000000000000000000000000000000000123456"),
            "data" -> JString("0xff33"),
            "topics" -> JArray(List(JString("0x33"), JString("0x55")))
          )
        )
      )
    )
  }
}
