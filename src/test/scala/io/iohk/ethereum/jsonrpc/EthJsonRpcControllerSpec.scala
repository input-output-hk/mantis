package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.JsonSerializers.{BlockResponseSerializer, QuantitiesSerializer}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.json4s.{DefaultFormats, Extraction}
import org.json4s.JsonAST._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class EthJsonRpcControllerSpec extends WordSpec with Matchers with PropertyChecks {

  implicit val format = DefaultFormats + BlockResponseSerializer + QuantitiesSerializer

  "JsonRpcController when sent eth_getBlockTransactionCountByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val request = JsonRpcRequest(
      "2.0",
      "eth_getBlockTransactionCountByHash",
      Some(JArray(List(JString(s"0x${Hex.toHexString(blockToRequestHash.toArray)}")))),
      Some(JInt(1))
    )

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)
      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JNull)
    }

    "return that the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
      blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)
      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JString("0x0"))
    }

    "return the correct number of txs when the requested block is in the blockchain and has some tx" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)
      val expectedTxCount = Extraction.decompose(BigInt(blockToRequest.body.transactionList.size))

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedTxCount)
    }

  }

  "JsonRpcController when sent eth_getBlockByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val blockTd = blockToRequest.header.difficulty
    val stxsViews = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    val request = JsonRpcRequest(
      "2.0",
      "eth_getBlockByHash",
      Some(JArray(List(JString(s"0x${Hex.toHexString(blockToRequestHash.toArray)}"), JBool(true)))),
      Some(JInt(1))
    )

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JNull)
    }

    "return the block view correctly when it's totalDifficulty is in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)

      val expectedBlockView = Extraction.decompose(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedBlockView)
    }

    "return the block view correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(jsonRpcController.handleRequest(request), Duration.Inf)

      val expectedBlockView = Extraction.decompose(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = None))

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedBlockView)
    }

    "return the block view correctly when the txs should be hashed" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val paramsWithTxHashedTrue = Some(JArray(List(JString(s"0x${Hex.toHexString(blockToRequestHash.toArray)}"), JBool(false))))
      val response = Await.result(jsonRpcController.handleRequest(request.copy(params = paramsWithTxHashedTrue)), Duration.Inf)

      val expectedBlockView = Extraction.decompose(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedBlockView)
    }

  }


  "JsonRpcController when sent eth_getUncleByBlockHashAndIndex" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val uncleIndexToRequest = 0

    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val uncleTd = uncle.difficulty
    val blockToRequestWithUncles = blockToRequest.copy(body = BlockBody(Nil, Seq(uncle)))
    val blockToRequestWithUnclesHash = blockToRequestWithUncles.header.hash

    def requestForIndex(uncleIndex: Int): JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "eth_getUncleByBlockHashAndIndex",
      Some(JArray(List(JString(s"0x${Hex.toHexString(blockToRequestHash.toArray)}"), JString(s"0x${Hex.toHexString(BigInt(uncleIndex).toByteArray)}")))),
      Some(JInt(1))
    )

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(jsonRpcController.handleRequest(requestForIndex(uncleIndexToRequest)), Duration.Inf)

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JNull)
    }

    "return None when there's no uncle" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(jsonRpcController.handleRequest(requestForIndex(uncleIndexToRequest)), Duration.Inf)

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(JNull)
    }

    "return None when there's no uncle in the requested index" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)

      val response1 = Await.result(jsonRpcController.handleRequest(requestForIndex(1)), Duration.Inf)
      response1.jsonrpc shouldBe "2.0"
      response1.id shouldBe JInt(1)
      response1.error shouldBe None
      response1.result shouldBe Some(JNull)

      val response2 = Await.result(jsonRpcController.handleRequest(requestForIndex(-1)), Duration.Inf)
      response2.jsonrpc shouldBe "2.0"
      response2.id shouldBe JInt(1)
      response2.error shouldBe None
      response2.result shouldBe Some(JNull)

    }

    "return the uncle block view correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      val response = Await.result(jsonRpcController.handleRequest(requestForIndex(uncleIndexToRequest)), Duration.Inf)

      val expectedUncleBlockView = Extraction.decompose(BlockResponse(uncle, None))
        .removeField{
          case ("transactions", _) => true
          case _ => false
        }

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedUncleBlockView)
    }

    "return the uncle block view correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      blockchain.save(uncle.hash, uncleTd)
      val response = Await.result(jsonRpcController.handleRequest(requestForIndex(uncleIndexToRequest)), Duration.Inf)

      val expectedUncleBlockView = Extraction.decompose(BlockResponse(uncle, Some(uncleTd)))
        .removeField{
          case ("transactions", _) => true
          case _ => false
        }

      response.jsonrpc shouldBe "2.0"
      response.id shouldBe JInt(1)
      response.error shouldBe None
      response.result shouldBe Some(expectedUncleBlockView)
    }
  }

  trait TestSetup {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)

    val web3Service = new Web3Service
    val ethService = new EthService(blockchain)
    val jsonRpcController = new JsonRpcController(web3Service, ethService)
  }

}
