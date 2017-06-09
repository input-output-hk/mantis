package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.{DefaultPatience, Fixtures, crypto}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, BlockchainImpl}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.MiningConfig
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.Await
import io.iohk.ethereum.jsonrpc.EthService.ProtocolVersionRequest
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, Ledger}
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, HashByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.Validators
import io.iohk.ethereum.vm.UInt256
import org.scalamock.scalatest.MockFactory
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._

class EthServiceSpec extends FlatSpec with Matchers with ScalaFutures with MockFactory with DefaultPatience {

  behavior of "EthService"

  it should "answer eth_blockNumber with the latest block number" in new TestSetup {
    val bestBlockNumber = 10
    (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val response = Await.result(ethService.bestBlockNumber(BestBlockNumberRequest()), Duration.Inf).right.get
    response.bestBlockNumber shouldEqual bestBlockNumber
  }

  it should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest())
    val protocolVersion = response.futureValue.right.get.value

    protocolVersion shouldEqual "0x3f"
    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual EthService.CurrentProtocolVersion
  }

  it should "answer eth_getBlockTransactionCountByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe None
  }

  it should "answer eth_getBlockTransactionCountByHash with the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
    blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe Some(0)
  }

  it should "answer eth_getBlockTransactionCountByHash correctly when the requested block is in the blockchain and has some tx" in new TestSetup {
    blockchain.save(blockToRequest)
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no block with the requested hash" in new TestSetup {
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf).right.get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no tx in requested index" in new TestSetup {
    blockchain.save(blockToRequest)

    val invalidTxIndex = blockToRequest.body.transactionList.size
    val requestWithInvalidIndex = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, invalidTxIndex)
    val response = Await.result(
      ethService.getTransactionByBlockHashAndIndexRequest(requestWithInvalidIndex),
      Duration.Inf
    ).right.get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with the transaction response correctly when the requested index has one" in new TestSetup {
    blockchain.save(blockToRequest)

    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf).right.get

    val requestedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = TransactionResponse(requestedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  //FIXME: This test should be changed once the pending block doesn't equal to the latest one
  it should "answer eth_getBlockByNumber with the correct parameters in None when the pending block is requested" in new TestSetup {
    val bestBlockNumber = 10

    blockchain.save(blockToRequest.copy(header = blockToRequest.header.copy(number = bestBlockNumber)))
    blockchain.save(blockToRequestHash, blockTd)
    (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val request = BlockByNumberRequest(BlockParam.Pending, fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get
    response.blockResponse.get.hash shouldBe None
    response.blockResponse.get.nonce shouldBe None
    response.blockResponse.get.miner shouldBe None
  }

  it should "answer eth_getBlockByNumber with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.totalDifficulty shouldBe None
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request.copy(fullTxs = false)), Duration.Inf).right.get

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getBlockByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get

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
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get

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
    val response = Await.result(ethService.getByBlockHash(request.copy(fullTxs = false)), Duration.Inf).right.get

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle" in new TestSetup {
    blockchain.save(blockToRequest)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response1 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1)), Duration.Inf).right.get
    val response2 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1)), Duration.Inf).right.get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockHashAndIndex correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)
    blockchain.save(uncle.hash, uncleTd)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd), pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle" in new TestSetup {
    blockchain.save(blockToRequest)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response1 = Await.result(ethService.getUncleByBlockNumberAndIndex(request.copy(uncleIndex = 1)), Duration.Inf).right.get
    val response2 = Await.result(ethService.getUncleByBlockNumberAndIndex(request.copy(uncleIndex = -1)), Duration.Inf).right.get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)
    blockchain.save(uncle.hash, uncleTd)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd), pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "return syncing info" in new TestSetup {
    (appStateStorage.getSyncStartingBlock _).expects().returning(999)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(10000)
    (appStateStorage.getBestBlockNumber _).expects().returning(200)
    val response = ethService.syncing(SyncingRequest()).futureValue.right.get

    response shouldEqual SyncingResponse(
      startingBlock = 999,
      currentBlock = 200,
      highestBlock = 10000
    )
  }

  it should "return requested work" in new TestSetup {
    (blockGenerator.generateBlockForMining _).expects(BigInt(1), Nil, *, *).returning(Right(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val response: ServiceResponse[GetWorkResponse] = ethService.getWork(GetWorkRequest())
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactions(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(1))
    ommersPool.reply(OmmersPool.Ommers(Nil))

    response.futureValue shouldEqual Right(GetWorkResponse(powHash, seedHash, target))
  }

  it should "accept submitted correct PoW" in new TestSetup {
    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(Some(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethService.submitWork(req)
    response.futureValue shouldEqual Right(SubmitWorkResponse(true))
  }

  it should "reject submitted correct PoW when header is no longer in cache" in new TestSetup {
    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(None)
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethService.submitWork(req)
    response.futureValue shouldEqual Right(SubmitWorkResponse(false))
  }

  it should "execute call and return a value" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.header.number)

    val txResult = TxResult(InMemoryWorldStateProxy(storagesInstance.storages), 123, Nil, ByteString("return_value"))
    (ledger.simulateTransaction _).expects(*, *, *).returning(txResult)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      1, 2, 3, ByteString(""))
    val response = ethService.call(CallRequest(tx, BlockParam.Latest))

    response.futureValue shouldEqual Right(CallResponse(ByteString("return_value")))
  }

  it should "get uncle count by block number" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.header.number)

    val response = ethService.getUncleCountByBlockNumber(GetUncleCountByBlockNumberRequest(BlockParam.Latest))

    response.futureValue shouldEqual Right(GetUncleCountByBlockNumberResponse(blockToRequest.body.uncleNodesList.size))
  }

  it should "get uncle count by block hash" in new TestSetup {
    blockchain.save(blockToRequest)

    val response = ethService.getUncleCountByBlockHash(GetUncleCountByBlockHashRequest(blockToRequest.header.hash))

    response.futureValue shouldEqual Right(GetUncleCountByBlockHashResponse(blockToRequest.body.uncleNodesList.size))
  }

  it should "get transaction count by block number" in new TestSetup {
    blockchain.save(blockToRequest)

    val response = ethService.getBlockTransactionCountByNumber(GetBlockTransactionCountByNumberRequest(BlockParam.WithNumber(blockToRequest.header.number)))

    response.futureValue shouldEqual Right(GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size))
  }

  it should "get transaction count by latest block number" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.header.number)

    val response = ethService.getBlockTransactionCountByNumber(GetBlockTransactionCountByNumberRequest(BlockParam.Latest))

    response.futureValue shouldEqual Right(GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size))
  }

  it should "handle getCode request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))
    storagesInstance.storages.evmCodeStorage.put(ByteString("code hash"), ByteString("code code code"))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.nodeStorage, (input: Array[Byte]) => crypto.kec256(input))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(0), ByteString(""), ByteString("code hash")))

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.header.number)

    val response = ethService.getCode(GetCodeRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetCodeResponse(ByteString("code code code")))
  }

  it should "accept and report hashrate" in new TestSetup {
    val rate: BigInt = 42
    val id = ByteString("id")

    ethService.submitHashRate(SubmitHashRateRequest(12, id)).futureValue shouldEqual Right(SubmitHashRateResponse(true))
    ethService.submitHashRate(SubmitHashRateRequest(rate, id)).futureValue shouldEqual Right(SubmitHashRateResponse(true))

    val response = ethService.getHashRate(GetHashRateRequest())
    response.futureValue shouldEqual Right(GetHashRateResponse(rate))
  }

  it should "combine hashrates from many miners and remove timed out rates" in new TestSetup {
    val rate: BigInt = 42
    val id1 = ByteString("id1")
    val id2 = ByteString("id2")

    ethService.submitHashRate(SubmitHashRateRequest(rate, id1)).futureValue shouldEqual Right(SubmitHashRateResponse(true))
    Thread.sleep(2.seconds.toMillis)
    ethService.submitHashRate(SubmitHashRateRequest(rate, id2)).futureValue shouldEqual Right(SubmitHashRateResponse(true))

    val response1 = ethService.getHashRate(GetHashRateRequest())
    response1.futureValue shouldEqual Right(GetHashRateResponse(rate * 2))

    Thread.sleep(4.seconds.toMillis)
    val response2 = ethService.getHashRate(GetHashRateRequest())
    response2.futureValue shouldEqual Right(GetHashRateResponse(rate))
  }

  it should "return if node is mining base on getWork" in new TestSetup {
    ethService.getMining(GetMiningRequest()).futureValue shouldEqual Right(GetMiningResponse(false))

    (blockGenerator.generateBlockForMining _).expects(*, *, *, *).returning(Right(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)
    ethService.getWork(GetWorkRequest())

    Thread.sleep(1.seconds.toMillis)

    val response = ethService.getMining(GetMiningRequest())

    response.futureValue shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining base on submitWork" in new TestSetup {
    ethService.getMining(GetMiningRequest()).futureValue shouldEqual Right(GetMiningResponse(false))

    (blockGenerator.getPrepared _).expects(*).returning(Some(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)
    ethService.submitWork(SubmitWorkRequest(ByteString("nonce"), ByteString(Hex.decode("01" * 32)), ByteString(Hex.decode("01" * 32))))

    Thread.sleep(1.seconds.toMillis)

    val response = ethService.getMining(GetMiningRequest())

    response.futureValue shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining base on submitHashRate" in new TestSetup {
    ethService.getMining(GetMiningRequest()).futureValue shouldEqual Right(GetMiningResponse(false))

    ethService.submitHashRate(SubmitHashRateRequest(42, ByteString("id")))

    Thread.sleep(1.seconds.toMillis)

    val response = ethService.getMining(GetMiningRequest())

    response.futureValue shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining after time out" in new TestSetup {
    (blockGenerator.generateBlockForMining _).expects(*, *, *, *).returning(Right(block))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)
    ethService.getWork(GetWorkRequest())

    Thread.sleep(6.seconds.toMillis)

    val response = ethService.getMining(GetMiningRequest())

    response.futureValue shouldEqual Right(GetMiningResponse(false))
  }

  it should "return correct coinbase" in new TestSetup {
    val response = ethService.getCoinbase(GetCoinbaseRequest())
    response.futureValue shouldEqual Right(GetCoinbaseResponse(miningConfig.coinbase))
  }

  it should "handle getBalance request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.nodeStorage, (input: Array[Byte]) => crypto.kec256(input))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(123), ByteString(""), ByteString("code hash")))

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.header.number)

    val response = ethService.getBalance(GetBalanceRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetBalanceResponse(123))
  }

  it should "handle getStorageAt request" in new TestSetup {
    import io.iohk.ethereum.rlp.UInt256RLPImplicits._

    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val byteArrayUInt256Serializer = new ByteArrayEncoder[UInt256] {
      override def toBytes(input: UInt256): Array[Byte] = input.bytes.toArray[Byte]
    }

    val rlpUInt256Serializer = new ByteArraySerializable[UInt256] {
      override def fromBytes(bytes: Array[Byte]): UInt256 = ByteString(bytes).toUInt256
      override def toBytes(input: UInt256): Array[Byte] = input.toBytes
    }

    val storageMpt =
      MerklePatriciaTrie[UInt256, UInt256](storagesInstance.storages.nodeStorage, crypto.kec256(_: Array[Byte]))(
        HashByteArraySerializable(byteArrayUInt256Serializer), rlpUInt256Serializer)
        .put(UInt256(333), UInt256(123))

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.nodeStorage, (input: Array[Byte]) => crypto.kec256(input))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(0), ByteString(storageMpt.getRootHash), ByteString("")))

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.header.number)

    val response = ethService.getStorageAt(GetStorageAtRequest(address, 333, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetStorageAtResponse(UInt256(123).bytes))
  }

  it should "handle get transaction count request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.nodeStorage, (input: Array[Byte]) => crypto.kec256(input))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(999, UInt256(0), ByteString(""), ByteString("")))

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.header.number)

    val response = ethService.getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetTransactionCountResponse(BigInt(999)))
  }

  trait TestSetup extends MockFactory {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)
    val blockGenerator = mock[BlockGenerator]
    val appStateStorage = mock[AppStateStorage]
    val keyStore = mock[KeyStore]
    val ledger = mock[Ledger]
    val validators = mock[Validators]
    val blockchainConfig = mock[BlockchainConfig]

    implicit val system = ActorSystem("EthServiceSpec_System")

    val syncingController = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()

    val miningConfig = new MiningConfig {
      override val coinbase: Address = Address(42)
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val txPoolSize: Int = 30
      override val poolingServicesTimeout: FiniteDuration = 3.seconds
    }

    val ethService = new EthService(storagesInstance.storages, blockGenerator, appStateStorage, miningConfig, ledger,
      blockchainConfig, keyStore, pendingTransactionsManager.ref, syncingController.ref, ommersPool.ref)

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestNumber = blockToRequest.header.number
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
    val seedHash = ByteString(Hex.decode("00" * 32))
    val powHash = ByteString(Hex.decode("f5877d30b85d6cd0f80d2c4711e3cfb7d386e331f801f903d9ca52fc5e8f7cc2"))
    val target = ByteString((BigInt(2).pow(256) / difficulty).toByteArray)
  }

}
