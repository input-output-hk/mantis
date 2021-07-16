package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString

import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.Duration

import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.mining.MiningConfigs
import io.iohk.ethereum.consensus.mining.TestMining
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.jsonrpc.EthBlocksService._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

class EthBlocksServiceSpec
    extends TestKit(ActorSystem("EthBlocksServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  "EthBlocksService" should "answer eth_blockNumber with the latest block number" in new TestSetup {
    val bestBlockNumber = 10
    blockchain.saveBestKnownBlocks(bestBlockNumber)

    val response = ethBlocksService.bestBlockNumber(BestBlockNumberRequest()).runSyncUnsafe(Duration.Inf).toOption.get
    response.bestBlockNumber shouldEqual bestBlockNumber
  }

  it should "answer eth_getBlockTransactionCountByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = ethBlocksService.getBlockTransactionCountByHash(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.txsQuantity shouldBe None
  }

  it should "answer eth_getBlockTransactionCountByHash with the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest.copy(body = BlockBody(Nil, Nil))).commit()
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = ethBlocksService.getBlockTransactionCountByHash(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.txsQuantity shouldBe Some(0)
  }

  it should "answer eth_getBlockTransactionCountByHash correctly when the requested block is in the blockchain and has some tx" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = ethBlocksService.getBlockTransactionCountByHash(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
  }

  it should "answer eth_getBlockByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = ethBlocksService.getByBlockHash(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's chain weight is in blockchain" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequestHash, blockWeight))
      .commit()

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = ethBlocksService.getByBlockHash(request).runSyncUnsafe(Duration.Inf).toOption.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(
      BlockResponse(blockToRequest, fullTxs = true, weight = Some(blockWeight))
    )
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(blockWeight)
    response.blockResponse.get.transactions.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's chain weight is not in blockchain" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = ethBlocksService.getByBlockHash(request).runSyncUnsafe(Duration.Inf).toOption.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe None
    response.blockResponse.get.transactions.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequestHash, blockWeight))
      .commit()

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response =
      ethBlocksService.getByBlockHash(request.copy(fullTxs = false)).runSyncUnsafe(Duration.Inf).toOption.get

    response.blockResponse shouldBe Some(
      BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight))
    )
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(blockWeight)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getBlockByNumber with the correct block when the pending block is requested" in new TestSetup {
    (appStateStorage.getBestBlockNumber _: () => BigInt).expects().returns(blockToRequest.header.number)

    (() => blockGenerator.getPendingBlockAndState)
      .expects()
      .returns(Some(PendingBlockAndState(PendingBlock(blockToRequest, Nil), fakeWorld)))

    val request = BlockByNumberRequest(BlockParam.Pending, fullTxs = true)
    val response = ethBlocksService.getBlockByNumber(request).runSyncUnsafe().toOption.get

    response.blockResponse.isDefined should be(true)
    val blockResponse = response.blockResponse.get

    blockResponse.hash shouldBe None
    blockResponse.nonce shouldBe None
    blockResponse.miner shouldBe None
    blockResponse.number shouldBe blockToRequest.header.number
  }

  it should "answer eth_getBlockByNumber with the latest block pending block is requested and there are no pending ones" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequestHash, blockWeight))
      .commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    (() => blockGenerator.getPendingBlockAndState).expects().returns(None)

    val request = BlockByNumberRequest(BlockParam.Pending, fullTxs = true)
    val response = ethBlocksService.getBlockByNumber(request).runSyncUnsafe().toOption.get
    response.blockResponse.get.hash.get shouldEqual blockToRequest.header.hash
  }

  it should "answer eth_getBlockByNumber with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = ethBlocksService.getBlockByNumber(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's chain weight is in blockchain" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequestHash, blockWeight))
      .commit()

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = ethBlocksService.getBlockByNumber(request).runSyncUnsafe(Duration.Inf).toOption.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(
      BlockResponse(blockToRequest, fullTxs = true, weight = Some(blockWeight))
    )
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(blockWeight)
    response.blockResponse.get.transactions.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's chain weight is not in blockchain" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = ethBlocksService.getBlockByNumber(request).runSyncUnsafe(Duration.Inf).toOption.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.header), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe None
    response.blockResponse.get.transactions.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequest)
      .and(blockchainWriter.storeChainWeight(blockToRequestHash, blockWeight))
      .commit()

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response =
      ethBlocksService.getBlockByNumber(request.copy(fullTxs = false)).runSyncUnsafe(Duration.Inf).toOption.get

    response.blockResponse shouldBe Some(
      BlockResponse(blockToRequest, fullTxs = false, weight = Some(blockWeight))
    )
    response.blockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(blockWeight)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "get transaction count by block number" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()

    val response = ethBlocksService.getBlockTransactionCountByNumber(
      GetBlockTransactionCountByNumberRequest(BlockParam.WithNumber(blockToRequest.header.number))
    )

    response.runSyncUnsafe() shouldEqual Right(
      GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size)
    )
  }

  it should "get transaction count by latest block number" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val response =
      ethBlocksService.getBlockTransactionCountByNumber(GetBlockTransactionCountByNumberRequest(BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(
      GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size)
    )
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequestWithUncles).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response1 =
      ethBlocksService
        .getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1))
        .runSyncUnsafe(Duration.Inf)
        .toOption
        .get
    val response2 =
      ethBlocksService
        .getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1))
        .runSyncUnsafe(Duration.Inf)
        .toOption
        .get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex correctly when the requested index has one but there's no chain weight for it" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequestWithUncles).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockHashAndIndex correctly when the requested index has one and there's chain weight for it" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequestWithUncles)
      .and(blockchainWriter.storeChainWeight(uncle.hash, uncleWeight))
      .commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleWeight), pendingBlock = false))
    response.uncleBlockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(uncleWeight)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle" in new TestSetup {

    blockchainWriter.storeBlock(blockToRequest).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle in the requested index" in new TestSetup {

    blockchainWriter.storeBlock(blockToRequestWithUncles).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response1 =
      ethBlocksService
        .getUncleByBlockNumberAndIndex(request.copy(uncleIndex = 1))
        .runSyncUnsafe(Duration.Inf)
        .toOption
        .get
    val response2 =
      ethBlocksService
        .getUncleByBlockNumberAndIndex(request.copy(uncleIndex = -1))
        .runSyncUnsafe(Duration.Inf)
        .toOption
        .get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one but there's no chain weight for it" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequestWithUncles).commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one and there's chain weight for it" in new TestSetup {
    blockchainWriter
      .storeBlock(blockToRequestWithUncles)
      .and(blockchainWriter.storeChainWeight(uncle.hash, uncleWeight))
      .commit()

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = ethBlocksService.getUncleByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleWeight), pendingBlock = false))
    response.uncleBlockResponse.get.asInstanceOf[BlockResponse].chainWeight shouldBe Some(uncleWeight)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "get uncle count by block number" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val response = ethBlocksService.getUncleCountByBlockNumber(GetUncleCountByBlockNumberRequest(BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(
      GetUncleCountByBlockNumberResponse(blockToRequest.body.uncleNodesList.size)
    )
  }

  it should "get uncle count by block hash" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()

    val response =
      ethBlocksService.getUncleCountByBlockHash(GetUncleCountByBlockHashRequest(blockToRequest.header.hash))

    response.runSyncUnsafe() shouldEqual Right(
      GetUncleCountByBlockHashResponse(blockToRequest.body.uncleNodesList.size)
    )
  }

  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup {
    val blockGenerator: PoWBlockGenerator = mock[PoWBlockGenerator]
    val appStateStorage: AppStateStorage = mock[AppStateStorage]

    override lazy val mining: TestMining = buildTestMining().withBlockGenerator(blockGenerator)
    override lazy val miningConfig = MiningConfigs.miningConfig

    lazy val ethBlocksService = new EthBlocksService(
      blockchain,
      blockchainReader,
      mining,
      blockQueue
    )

    val blockToRequest: Block = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val blockToRequestNumber = blockToRequest.header.number
    val blockToRequestHash = blockToRequest.header.hash
    val blockWeight: ChainWeight = ChainWeight.totalDifficultyOnly(blockToRequest.header.difficulty)

    val uncle = Fixtures.Blocks.DaoForkBlock.header
    val uncleWeight: ChainWeight = ChainWeight.totalDifficultyOnly(uncle.difficulty)
    val blockToRequestWithUncles: Block = blockToRequest.copy(body = BlockBody(Nil, Seq(uncle)))

    val fakeWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString.empty,
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )
  }
}
