package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//FIXME: Use Async and move to JsonRpcControllerSpec (?)
class EthServiceSpec extends WordSpec with Matchers with PropertyChecks {

  def validateResponse[A](responseFuture: Future[A])(isValid: A => Unit): Unit = {
    responseFuture.onComplete { response =>
      assert(response.isSuccess)
      response.map(isValid)
    }
  }

  "eth_getBlockTransactionCountByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val request = TxCountByBlockHashRequest(blockToRequestHash)

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val responseFuture = ethService.getBlockTransactionCountByHash(request)
      validateResponse(responseFuture) { response =>
        response.txsQuantity shouldBe None
      }
    }

    "return that the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
      blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
      val responseFuture = ethService.getBlockTransactionCountByHash(request)
      validateResponse(responseFuture) { response =>
        response.txsQuantity shouldBe Some(0)
      }
    }

    "return the correct number of txs when the requested block is in the blockchain and has some tx" in new TestSetup {
      blockchain.save(blockToRequest)
      val responseFuture = ethService.getBlockTransactionCountByHash(request)
      validateResponse(responseFuture) { response =>
        response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
      }
    }

  }

  "eth_getBlockByHash" should {

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val blockTd = blockToRequest.header.difficulty
    val stxsViews = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      SignedTransactionView(stx, Some(blockToRequest), Some(txIndex))
    }

    val request = BlockByBlockHashRequest(blockToRequestHash, txHashed = false)

    "return None when the requested block isn't in the blockchain" in new TestSetup {
      val responseFuture = ethService.getByBlockHash(request)
      validateResponse(responseFuture) { response =>
        response.blockView shouldBe None
      }
    }

    "return the block view correctly when it's totalDifficulty is in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val responseFuture = ethService.getByBlockHash(request)

      validateResponse(responseFuture) { response =>
        response.blockView shouldBe Some(BlockView(blockToRequest, txHashed = false, totalDifficulty = Some(blockTd)))
        response.blockView.get.totalDifficulty shouldBe Some(blockTd)
        response.blockView.get.transactions.right.toOption shouldBe Some(stxsViews)
      }
    }

    "return the block view correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
      blockchain.save(blockToRequest)
      val responseFuture = ethService.getByBlockHash(request)

      validateResponse(responseFuture) { response =>
        response.blockView shouldBe Some(BlockView(blockToRequest, txHashed = false))
        response.blockView.get.totalDifficulty shouldBe None
        response.blockView.get.transactions.right.toOption shouldBe Some(stxsViews)
      }
    }

    "return the block view correctly when the txs should be hashed" in new TestSetup {
      blockchain.save(blockToRequest)
      blockchain.save(blockToRequestHash, blockTd)
      val responseFuture = ethService.getByBlockHash(request.copy(txHashed = true))

      validateResponse(responseFuture) { response =>
        response.blockView shouldBe Some(BlockView(blockToRequest, txHashed = true, totalDifficulty = Some(blockTd)))
        response.blockView.get.totalDifficulty shouldBe Some(blockTd)
        response.blockView.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
      }
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
      val responseFuture = ethService.getUncleByBlockHashAndIndex(request)
      validateResponse(responseFuture) { response =>
        response.uncleBlockView shouldBe None
      }
    }

    "return None when there's no uncle" in new TestSetup {
      blockchain.save(blockToRequest)
      val responseFuture = ethService.getUncleByBlockHashAndIndex(request)

      validateResponse(responseFuture) { response =>
        response.uncleBlockView shouldBe None
      }
    }

    "return None when there's no uncle in the requested index" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)

      val responseFuture1 = ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1))
      val responseFuture2 = ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1))

      validateResponse(responseFuture1) { response =>
        response.uncleBlockView shouldBe None
      }
      validateResponse(responseFuture2) { response =>
        response.uncleBlockView shouldBe None
      }
    }

    "return the uncle block view correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      val responseFuture = ethService.getUncleByBlockHashAndIndex(request)

      validateResponse(responseFuture) { response =>
        response.uncleBlockView shouldBe Some(BlockView(uncle, None))
        response.uncleBlockView.get.totalDifficulty shouldBe None
        response.uncleBlockView.get.transactions shouldBe Right(Nil)
        response.uncleBlockView.get.unclesHash shouldBe Nil
      }
    }

    "return the uncle block view correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
      blockchain.save(blockToRequestWithUncles)
      blockchain.save(uncle.hash, uncleTd)
      val responseFuture = ethService.getUncleByBlockHashAndIndex(request)

      validateResponse(responseFuture) { response =>
        response.uncleBlockView shouldBe Some(BlockView(uncle, Some(uncleTd)))
        response.uncleBlockView.get.totalDifficulty shouldBe Some(uncleTd)
        response.uncleBlockView.get.transactions shouldBe Right(Nil)
        response.uncleBlockView.get.unclesHash shouldBe Nil
      }
    }
  }

  trait TestSetup {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)

    val ethService = new EthService(blockchain)
  }

}
