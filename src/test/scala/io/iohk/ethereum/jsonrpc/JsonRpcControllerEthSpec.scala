package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.validators.SignedTransactionValidator
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.FilterManager.{LogFilterLogs, TxLog}
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.ommers.OmmersPool.Ommers
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.{Fixtures, LongPatience, Timeouts}
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Extraction, Formats}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Future

// scalastyle:off file.size.limit
// scalastyle:off magic.number
class JsonRpcControllerEthSpec
    extends AnyFlatSpec
    with Matchers
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  it should "eth_protocolVersion" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("eth_protocolVersion")
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult("0x3f")
  }

  it should "handle eth_chainId" in new JsonRpcControllerFixture {
    val request = newJsonRpcRequest("eth_chainId")
    val response = jsonRpcController.handleRequest(request).futureValue

    response should haveStringResult("0x3d")
  }

  it should "handle eth_blockNumber request" in new JsonRpcControllerFixture {
    val bestBlockNumber = 10
    blockchain.saveBestKnownBlocks(bestBlockNumber)

    val rpcRequest = newJsonRpcRequest("eth_blockNumber")
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult(s"0xa")
  }

  it should "eth_syncing" in new JsonRpcControllerFixture {
    (appStateStorage.getSyncStartingBlock _).expects().returning(100)
    (appStateStorage.getBestBlockNumber _).expects().returning(200)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(300)

    blockchain.saveBestKnownBlocks(200)
    val rpcRequest = newJsonRpcRequest("eth_syncing")

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveObjectResult(
      "startingBlock" -> "0x64",
      "currentBlock" -> "0xc8",
      "highestBlock" -> "0x12c"
    )
  }

  it should "handle eth_getBlockTransactionCountByHash request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    blockchain.storeBlock(blockToRequest).commit()

    val rpcRequest = newJsonRpcRequest(
      "eth_getBlockTransactionCountByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}")),
    )
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    val expectedTxCount = Extraction.decompose(BigInt(blockToRequest.body.transactionList.size))
    response should haveResult(expectedTxCount)
  }

  it should "handle eth_getBlockByHash request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockTd = blockToRequest.header.difficulty

    blockchain
      .storeBlock(blockToRequest)
      .and(blockchain.storeTotalDifficulty(blockToRequest.header.hash, blockTd))
      .commit()

    val request = newJsonRpcRequest(
      "eth_getBlockByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}"), JBool(false)),
    )
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getBlockByNumber request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockTd = blockToRequest.header.difficulty

    blockchain
      .storeBlock(blockToRequest)
      .and(blockchain.storeTotalDifficulty(blockToRequest.header.hash, blockTd))
      .commit()

    val request = newJsonRpcRequest(
      "eth_getBlockByNumber",
      List(JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"), JBool(false))
    )
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedBlockResponse =
      Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))

    response should haveResult(expectedBlockResponse)
  }

  it should "handle eth_getUncleByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedUncleBlockResponse = Extraction
      .decompose(BlockResponse(uncle, None, pendingBlock = false))
      .removeField {
        case ("transactions", _) => true
        case _ => false
      }

    response should haveResult(expectedUncleBlockResponse)
  }

  it should "handle eth_getUncleByBlockNumberAndIndex request" in new JsonRpcControllerFixture {
    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, BlockBody(Nil, Seq(uncle)))

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleByBlockNumberAndIndex",
      List(
        JString(s"0x${Hex.toHexString(blockToRequest.header.number.toByteArray)}"),
        JString(s"0x${Hex.toHexString(BigInt(0).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue

    val expectedUncleBlockResponse = Extraction
      .decompose(BlockResponse(uncle, None, pendingBlock = false))
      .removeField {
        case ("transactions", _) => true
        case _ => false
      }

    response should haveResult(expectedUncleBlockResponse)
  }

  it should "handle eth_getTransactionByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(txIndexToRequest).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "handle eth_getRawTransactionByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(txIndexToRequest).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndexToRequest)

    response should haveResult(expectedTxResponse)
  }

  it should "handle eth_getRawTransactionByHash request" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val txResponse: SignedTransaction = Fixtures.Blocks.Block3125369.body.transactionList.head
    (mockEthService.getRawTransactionByHash _)
      .expects(*)
      .returning(Future.successful(Right(RawTransactionResponse(Some(txResponse)))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByHash",
      List(
        JString("0xe9b2d3e8a2bc996a1c7742de825fdae2466ae783ce53484304efffe304ff232d")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(encodeSignedTrx(txResponse))
  }

  it should "eth_sendTransaction" in new JsonRpcControllerFixture {
    val params = JObject(
      "from" -> Address(42).toString,
      "to" -> Address(123).toString,
      "value" -> 1000
    ) :: Nil

    val txHash = ByteString(1, 2, 3, 4)

    (personalService
      .sendTransaction(_: SendTransactionRequest))
      .expects(*)
      .returning(Future.successful(Right(SendTransactionResponse(txHash))))

    val rpcRequest = newJsonRpcRequest("eth_sendTransaction", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveResult(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "eth_getWork" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    blockchain.save(parentBlock, Nil, parentBlock.header.difficulty, true)
    (blockGenerator.generateBlock _)
      .expects(parentBlock, *, *, *)
      .returns(Right(PendingBlock(Block(blockHeader, BlockBody(Nil, Nil)), Nil)))

    val request: JsonRpcRequest = newJsonRpcRequest("eth_getWork")

    val result: Future[JsonRpcResponse] = jsonRpcController.handleRequest(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    ommersPool.reply(Ommers(Nil))

    val response = result.futureValue
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
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

    val seed = s"""0x${"00" * 32}"""
    val target = "0x1999999999999999999999999999999999999999999999999999999999999999"
    val headerPowHash = s"0x${Hex.toHexString(kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)))}"

    blockchain.save(parentBlock, Nil, parentBlock.header.difficulty, true)
    (blockGenerator.generateBlock _)
      .expects(parentBlock, *, *, *)
      .returns(Right(PendingBlock(Block(blockHeader, BlockBody(Nil, Nil)), Nil)))

    val request: JsonRpcRequest = newJsonRpcRequest("eth_getWork")

    val result: Future[JsonRpcResponse] = jsonRpcController.handleRequest(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    //on time out it should respond with empty list

    val response = result.futureValue(timeout(Timeouts.longTimeout))
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
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

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

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveBooleanResult(true)
  }

  it should "eth_submitHashrate" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_submitHashrate",
      List(
        JString(s"0x${"0" * 61}500"),
        JString(s"0x59daa26581d0acd1fce254fb7e85952f4c09d0915afd33d3886cd914bc7d283c")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveBooleanResult(true)
  }

  it should "eth_hashrate" in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

    val request: JsonRpcRequest = newJsonRpcRequest("eth_hashrate")

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x0")
  }

  it should "eth_gasPrice" in new JsonRpcControllerFixture {
    blockchain
      .storeBlock(Block(Fixtures.Blocks.Block3125369.header.copy(number = 42), Fixtures.Blocks.Block3125369.body))
      .commit()
    blockchain.saveBestKnownBlocks(42)

    val request: JsonRpcRequest = newJsonRpcRequest("eth_gasPrice")

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x4a817c800")
  }

  it should "eth_call" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.call _).expects(*).returning(Future.successful(Right(CallResponse(ByteString("asd")))))

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
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult("0x617364")
  }

  it should "eth_estimateGas" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.estimateGas _)
      .expects(*)
      .anyNumberOfTimes()
      .returning(Future.successful(Right(EstimateGasResponse(2310))))

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
      val response = jsonRpcController.handleRequest(rpcRequest).futureValue

      response should haveStringResult("0x906")
    }

  }

  it should "eth_getCode" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getCode _)
      .expects(*)
      .returning(Future.successful(Right(GetCodeResponse(ByteString(Hex.decode("FFAA22"))))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getCode",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0xffaa22")
  }

  it should "eth_getUncleCountByBlockNumber" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getUncleCountByBlockNumber _)
      .expects(*)
      .returning(Future.successful(Right(GetUncleCountByBlockNumberResponse(2))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleCountByBlockNumber",
      List(
        JString(s"0x12")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x2")
  }

  it should "eth_getUncleCountByBlockHash " in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getUncleCountByBlockHash _)
      .expects(*)
      .returning(Future.successful(Right(GetUncleCountByBlockHashResponse(3))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getUncleCountByBlockHash",
      List(
        JString(s"0x7dc64cb9d8a95763e288d71088fe3116e10dbff317c09f7a9bd5dd6974d27d20")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x3")
  }

  it should "eth_getBlockTransactionCountByNumber " in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getBlockTransactionCountByNumber _)
      .expects(*)
      .returning(Future.successful(Right(GetBlockTransactionCountByNumberResponse(17))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getBlockTransactionCountByNumber",
      List(
        JString(s"0x123")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x11")
  }

  it should "eth_coinbase " in new JsonRpcControllerFixture {
    // Just record the fact that this is going to be called, we do not care about the returned value
    (validators.signedTransactionValidator _: (() => SignedTransactionValidator))
      .expects()
      .returns(null)
      .anyNumberOfTimes()

    (ledger.consensus _: (() => Consensus)).expects().returns(consensus).anyNumberOfTimes()

    val request: JsonRpcRequest = newJsonRpcRequest("eth_coinbase")

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x000000000000000000000000000000000000002a")
  }

  it should "eth_getTransactionByBlockNumberAndIndex by tag" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JString(s"latest"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by hex number" in new JsonRpcControllerFixture {
    val blockToRequest =
      Block(Fixtures.Blocks.Block3125369.header.copy(number = BigInt(0xc005)), Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JString(s"0xC005"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by number" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JInt(Fixtures.Blocks.Block3125369.header.number),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by tag" in new JsonRpcControllerFixture {
    // given
    val blockToRequest: Block = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JString(s"latest"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )

    // when
    val response = jsonRpcController.handleRequest(request).futureValue

    // then
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by hex number" in new JsonRpcControllerFixture {
    // given
    val blockToRequest =
      Block(Fixtures.Blocks.Block3125369.header.copy(number = BigInt(0xc005)), Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JString(s"0xC005"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )

    // when
    val response = jsonRpcController.handleRequest(request).futureValue

    // then
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by number" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JInt(Fixtures.Blocks.Block3125369.header.number),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getBalance" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getBalance _)
      .expects(*)
      .returning(Future.successful(Right(GetBalanceResponse(17))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getBalance",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x11")
  }

  it should "eth_getStorageAt" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getStorageAt _)
      .expects(*)
      .returning(Future.successful(Right(GetStorageAtResponse(ByteString("response")))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getStorageAt",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"0x01"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(JString("0x" + Hex.toHexString(ByteString("response").toArray[Byte])))
  }

  it should "eth_getTransactionCount" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getTransactionCount _)
      .expects(*)
      .returning(Future.successful(Right(GetTransactionCountResponse(123))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionCount",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x7b")
  }

  it should "eth_getTransactionByHash" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val txResponse = TransactionResponse(Fixtures.Blocks.Block3125369.body.transactionList.head)
    (mockEthService.getTransactionByHash _)
      .expects(*)
      .returning(Future.successful(Right(GetTransactionByHashResponse(Some(txResponse)))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByHash",
      List(
        JString("0xe9b2d3e8a2bc996a1c7742de825fdae2466ae783ce53484304efffe304ff232d")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(Extraction.decompose(txResponse))
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
      .returns(Future.successful(Right(SignResponse(sig))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_sign",
      List(
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString(s"0xdeadbeaf")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult(
      "0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"
    )
  }

  it should "eth_newFilter" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.newFilter _)
      .expects(*)
      .returning(Future.successful(Right(NewFilterResponse(123))))

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

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x7b")
  }

  it should "eth_newBlockFilter" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.newBlockFilter _)
      .expects(*)
      .returning(Future.successful(Right(NewFilterResponse(999))))

    val request: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_newBlockFilter",
      Some(JArray(List())),
      Some(JInt(1))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x3e7")
  }

  it should "eth_newPendingTransactionFilter" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.newPendingTransactionFilter _)
      .expects(*)
      .returning(Future.successful(Right(NewFilterResponse(2))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_newPendingTransactionFilter",
      Nil
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x2")
  }

  it should "eth_uninstallFilter" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.uninstallFilter _)
      .expects(*)
      .returning(Future.successful(Right(UninstallFilterResponse(true))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_uninstallFilter",
      List(JString("0x1"))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveBooleanResult(true)
  }

  it should "eth_getFilterChanges" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getFilterChanges _)
      .expects(*)
      .returning(
        Future.successful(
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

    val response = jsonRpcController.handleRequest(request).futureValue
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

  it should "eth_getFilterLogs" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getFilterLogs _)
      .expects(*)
      .returning(
        Future.successful(
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

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(JArray(List(JString("0x1234"), JString("0x4567"), JString("0x7890"))))
  }

  it should "eth_getLogs" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getLogs _)
      .expects(*)
      .returning(
        Future.successful(
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

    val response = jsonRpcController.handleRequest(request).futureValue
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

  it should "eth_getTransactionReceipt" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val arbitraryValue = 42

    val mockRequest = GetTransactionReceiptRequest(
      ByteString(Hex.decode("b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))
    )

    val mockResponse = Right(
      GetTransactionReceiptResponse(
        Some(
          TransactionReceiptResponse(
            transactionHash = ByteString(Hex.decode("23" * 32)),
            transactionIndex = 1,
            blockNumber = Fixtures.Blocks.Block3125369.header.number,
            blockHash = Fixtures.Blocks.Block3125369.header.hash,
            cumulativeGasUsed = arbitraryValue * 10,
            gasUsed = arbitraryValue,
            contractAddress = Some(Address(arbitraryValue)),
            logs = Seq(
              TxLog(
                logIndex = 0,
                transactionIndex = 1,
                transactionHash = ByteString(Hex.decode("23" * 32)),
                blockHash = Fixtures.Blocks.Block3125369.header.hash,
                blockNumber = Fixtures.Blocks.Block3125369.header.number,
                address = Address(arbitraryValue),
                data = ByteString(Hex.decode("43" * 32)),
                topics = Seq(ByteString(Hex.decode("44" * 32)), ByteString(Hex.decode("45" * 32)))
              )
            )
          )
        )
      )
    )

    (mockEthService.getTransactionReceipt _).expects(*).returning(Future.successful(mockResponse))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionReceipt",
      List(JString(s"0xb903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(
      JObject(
        JField("transactionHash", JString("0x" + "23" * 32)),
        JField("transactionIndex", JString("0x1")),
        JField("blockNumber", JString("0x2fb079")),
        JField("blockHash", JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))),
        JField("cumulativeGasUsed", JString("0x1a4")),
        JField("gasUsed", JString("0x2a")),
        JField("contractAddress", JString("0x000000000000000000000000000000000000002a")),
        JField(
          "logs",
          JArray(
            List(
              JObject(
                JField("logIndex", JString("0x0")),
                JField("transactionIndex", JString("0x1")),
                JField("transactionHash", JString("0x" + "23" * 32)),
                JField(
                  "blockHash",
                  JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))
                ),
                JField("blockNumber", JString("0x2fb079")),
                JField("address", JString("0x000000000000000000000000000000000000002a")),
                JField("data", JString("0x" + "43" * 32)),
                JField("topics", JArray(List(JString("0x" + "44" * 32), JString("0x" + "45" * 32))))
              )
            )
          )
        )
      )
    )
  }
}
