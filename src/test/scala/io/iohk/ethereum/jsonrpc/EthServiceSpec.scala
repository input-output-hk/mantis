package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, crypto}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import io.iohk.ethereum.jsonrpc.EthService.ProtocolVersionRequest
import io.iohk.ethereum.mining.BlockGenerator
import org.scalamock.scalatest.MockFactory
import org.spongycastle.util.encoders.Hex

class EthServiceSpec extends FlatSpec with Matchers with ScalaFutures with MockFactory {

  behavior of "EthService"

  it should "answer eth_blockNumber with the latest block number" in new TestSetup {
    val bestBlockNumber = 10
    (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val response = Await.result(ethService.bestBlockNumber(BestBlockNumberRequest()), Duration.Inf)
    response.bestBlockNumber shouldEqual bestBlockNumber
  }

  it should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest())
    val protocolVersion = response.futureValue.value

    protocolVersion shouldEqual "0x3f"
    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual EthService.CurrentProtocolVersion
  }

  it should "answer eth_getBlockTransactionCountByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
    response.txsQuantity shouldBe None
  }

  it should "answer eth_getBlockTransactionCountByHash with the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
    blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
    response.txsQuantity shouldBe Some(0)
  }

  it should "answer eth_getBlockTransactionCountByHash correctly when the requested block is in the blockchain and has some tx" in new TestSetup {
    blockchain.save(blockToRequest)
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf)
    response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no block with the requested hash" in new TestSetup {
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf)

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no tx in requested index" in new TestSetup {
    blockchain.save(blockToRequest)

    val invalidTxIndex = blockToRequest.body.transactionList.size
    val requestWithInvalidIndex = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, invalidTxIndex)
    val response = Await.result(
      ethService.getTransactionByBlockHashAndIndexRequest(requestWithInvalidIndex),
      Duration.Inf
    )

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with the transaction response correctly when the requested index has one" in new TestSetup {
    blockchain.save(blockToRequest)

    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf)

    val requestedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = TransactionResponse(requestedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  it should "answer eth_getBlockByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf)

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.totalDifficulty shouldBe None
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request.copy(fullTxs = false)), Duration.Inf)

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle" in new TestSetup {
    blockchain.save(blockToRequest)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response1 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1)), Duration.Inf)
    val response2 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1)), Duration.Inf)

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None))
    response.uncleBlockResponse.get.totalDifficulty shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockHashAndIndex correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)
    blockchain.save(uncle.hash, uncleTd)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf)

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd)))
    response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "return syncing info" in new TestSetup {
    (appStateStorage.getSyncStartingBlock _).expects().returning(999)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(10000)
    (appStateStorage.getBestBlockNumber _).expects().returning(200)
    val response = ethService.syncing(SyncingRequest())

    response.futureValue shouldEqual SyncingResponse(
      startingBlock = 999,
      currentBlock = 200,
      highestBlock = 10000
    )
  }

  it should "return requested work" in new TestSetup {
    (blockGenerator.generateBlockForMining _).expects(BigInt(1), *, *, *).returning(Right(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val response = ethService.getWork(GetWorkRequest())

    response.futureValue shouldEqual GetWorkResponse(powHash, seedHash, target)
  }

  trait TestSetup extends MockFactory {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)
    val blockGenerator = mock[BlockGenerator]
    val appStateStorage = mock[AppStateStorage]
    implicit val system = ActorSystem("EthServiceSpec_System")
    val syncingController = TestProbe()

    val ethService = new EthService(blockchain, blockGenerator, appStateStorage, syncingController.ref)

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestHash = blockToRequest.header.hash
    val blockTd = blockToRequest.header.difficulty

    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val uncleTd = uncle.difficulty
    val blockToRequestWithUncles = blockToRequest.copy(body = BlockBody(Nil, Seq(uncle)))
    val difficulty = 131072
    val block = Block(
      header = BlockHeader(
        parentHash = ByteString(Hex.decode("fae40e0347c422194d9a0abd00e76774dd85b607ac8614b9bb0abd09ceee8df2")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("000000000000000000000000000000000000002a")),
        stateRoot = ByteString(Hex.decode("2627314387b135a548040d3ca99dbf308265a3f9bd9246bee3e34d12ea9ff0dc")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = difficulty,
        number = 1,
        gasLimit = 16733003,
        gasUsed = 0,
        unixTimestamp = 1494604913,
        extraData = ByteString(Hex.decode("6d696e6564207769746820657463207363616c61")),
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ),
      body = BlockBody(Nil, Nil)
    )
    val mixHash = ByteString(Hex.decode("40d9bd2064406d7f22390766d6fe5eccd2a67aa89bf218e99df35b2dbb425fb1"))
    val nonce = ByteString(Hex.decode("ce1b500070aeec4f"))
    val seedHash = ByteString(crypto.kec256(Hex.decode("00" * 32)))
    val powHash = ByteString(Hex.decode("f5877d30b85d6cd0f80d2c4711e3cfb7d386e331f801f903d9ca52fc5e8f7cc2"))
    val target = ByteString((BigInt(2).pow(256) / difficulty).toByteArray)
  }

}
