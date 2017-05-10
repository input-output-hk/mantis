package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await

class EthServiceSpec extends WordSpec with Matchers with PropertyChecks {

  "eth_getBlockTransactionCountByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val request = TxCountByBlockHashRequest(blockToRequestHash)

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
      response.txsQuantity shouldBe None
    }

    "return that the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
      blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
      val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
      response.txsQuantity shouldBe Some(0)
    }

    "return the correct number of txs when the requested block is in the blockchain and has some tx" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
      response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
    }

  }

  "eth_getBlockByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val blockTd = blockToRequest.header.difficulty
    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)
      response.blockResponse shouldBe None
    }

    "return the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)

      response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))
      response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
      response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
    }

    "return the block response correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)

      response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
      response.blockResponse.get.totalDifficulty shouldBe None
      response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
    }

    "return the block response correctly when the txs should be hashed" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val response = Await.result(ethService.getByBlockHash(request.copy(fullTxs = false)), Duration.Inf)

      response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
      response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
      response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
    }

  }


  "eth_getUncleByBlockHashAndIndex" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)

    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val uncleTd = uncle.difficulty
    val blockToRequestWithUncles = blockToRequest.copy(body = BlockBody(Nil, Seq(uncle)))
    val blockToRequestWithUnclesHash = blockToRequestWithUncles.header.hash
    val requestForBlockWithUncles = request.copy(blockToRequestWithUnclesHash)

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)
      response.uncleBlockResponse shouldBe None
    }

    "return None when there's no uncle" in new TestSetup {
      blockchain.save(blockToRequest)
      val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

      response.uncleBlockResponse shouldBe None
    }

    "return None when there's no uncle in the requested index" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)

      val response1 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1)), Duration.Inf)
      val response2 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1)), Duration.Inf)

      response1.uncleBlockResponse shouldBe None
      response2.uncleBlockResponse shouldBe None
    }

    "return the uncle block response correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

      response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None))
      response.uncleBlockResponse.get.totalDifficulty shouldBe None
      response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
      response.uncleBlockResponse.get.uncles shouldBe Nil
    }

    "return the uncle block response correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      blockchain.save(uncle.hash, uncleTd)
      val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

      response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd)))
      response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
      response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
      response.uncleBlockResponse.get.uncles shouldBe Nil
    }
  }

  trait TestSetup {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)

    val ethService = new EthService(blockchain)
  }

}
