package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.domain.{Block, BlockBody, BlockchainImpl, BlockchainReader, Checkpoint}
import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.{Fixtures, NormalPatience, WithActorSystemShutDown}
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CheckpointingServiceSpec
    extends TestKit(ActorSystem("CheckpointingServiceSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with MockFactory
    with ScalaFutures
    with NormalPatience
    with ScalaCheckPropertyChecks
    with Matchers {

  "CheckpointService" should "get latest block (at a correct checkpointing interval) from Blockchain" in new TestSetup {
    val nums = for {
      k <- Gen.choose[Int](1, 10) // checkpointing interval
      m <- Gen.choose(0, 1000) // number of checkpoints in the chain
      n <- Gen.choose(0, k - 1) // distance from best block to checkpointed block
    } yield (k, m, n)

    forAll(nums) { case (k, m, n) =>
      val checkpointedBlockNum: BigInt = k * m
      val bestBlockNum: BigInt = checkpointedBlockNum + n

      val block = Block(Fixtures.Blocks.ValidBlock.header.copy(number = checkpointedBlockNum), BlockBody.empty)

      val request = GetLatestBlockRequest(k, None)
      val expectedResponse = GetLatestBlockResponse(Some(BlockInfo(block.hash, block.number)))

      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchainReader.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
      val result = service.getLatestBlock(request)

      result.runSyncUnsafe() shouldEqual Right(expectedResponse)
    }
  }

  it should "get latest block that is a descendant of the passed parent checkpoint block" in new TestSetup {
    val nums = for {
      k <- Gen.choose[Int](1, 10) // checkpointing interval
      m <- Gen.choose(0, 1000) // number of checkpoints in the chain
      n <- Gen.choose(0, k - 1) // distance from best block to checkpointed block
    } yield (k, m, n)

    val previousCheckpoint = Fixtures.Blocks.Block3125369.block
    val hash = previousCheckpoint.hash

    forAll(nums) { case (k, m, n) =>
      val checkpointedBlockNum: BigInt = k * m
      val bestBlockNum: BigInt = checkpointedBlockNum + n

      val block = Block(Fixtures.Blocks.ValidBlock.header.copy(number = checkpointedBlockNum), BlockBody.empty)

      val request = GetLatestBlockRequest(k, Some(hash))
      val expectedResponse = GetLatestBlockResponse(Some(BlockInfo(block.hash, block.number)))

      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchainReader.getBlockHeaderByHash _)
        .expects(hash)
        .returning(Some(previousCheckpoint.header.copy(number = 0)))
      (blockchainReader.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
      val result = service.getLatestBlock(request)

      result.runSyncUnsafe() shouldEqual Right(expectedResponse)
    }
  }

  it should "not return a block that is at the same height as the passed parent checkpoint block" in new TestSetup {
    val nums = for {
      k <- Gen.choose[Int](1, 10) // checkpointing interval
      m <- Gen.choose(0, 1000) // number of checkpoints in the chain
      n <- Gen.choose(0, k - 1) // distance from best block to checkpointed block
    } yield (k, m, n)

    val previousCheckpoint = Fixtures.Blocks.ValidBlock.block
    val hash = previousCheckpoint.hash

    forAll(nums) { case (k, m, n) =>
      val checkpointedBlockNum: BigInt = k * m
      val bestBlockNum: BigInt = checkpointedBlockNum + n

      val request = GetLatestBlockRequest(k, Some(hash))
      val expectedResponse = GetLatestBlockResponse(None)

      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchainReader.getBlockHeaderByHash _)
        .expects(hash)
        .returning(Some(previousCheckpoint.header.copy(number = bestBlockNum)))
      (blockchainReader.getBlockByNumber _).expects(*).returning(Some(previousCheckpoint))
      val result = service.getLatestBlock(request)

      result.runSyncUnsafe() shouldEqual Right(expectedResponse)
    }
  }

  it should "return an empty response if the descendant is not a part of a local blockchain" in new TestSetup {
    val nums = for {
      k <- Gen.choose[Int](1, 10) // checkpointing interval
      m <- Gen.choose(0, 1000) // number of checkpoints in the chain
      n <- Gen.choose(0, k - 1) // distance from best block to checkpointed block
    } yield (k, m, n)

    val previousCheckpoint = Fixtures.Blocks.ValidBlock.block
    val hash = previousCheckpoint.hash

    forAll(nums) { case (k, m, n) =>
      val checkpointedBlockNum: BigInt = k * m
      val bestBlockNum: BigInt = checkpointedBlockNum + n

      val block = Block(Fixtures.Blocks.ValidBlock.header.copy(number = checkpointedBlockNum), BlockBody.empty)

      val request = GetLatestBlockRequest(k, Some(hash))
      val expectedResponse = GetLatestBlockResponse(None)

      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchainReader.getBlockHeaderByHash _).expects(hash).returning(None)
      (blockchainReader.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
      val result = service.getLatestBlock(request)

      result.runSyncUnsafe() shouldEqual Right(expectedResponse)
    }
  }

  it should "send new checkpoint to Sync" in new TestSetup {
    val parentBlock = Fixtures.Blocks.ValidBlock.block
    val hash = parentBlock.hash
    val signatures = Nil
    val request = PushCheckpointRequest(hash, signatures)
    val expectedResponse = PushCheckpointResponse()

    (ledger.getBlockByHash _).expects(hash).returning(Some(parentBlock)).once()

    val result = service.pushCheckpoint(request).runSyncUnsafe()
    val checkpointBlock = checkpointBlockGenerator.generate(parentBlock, Checkpoint(signatures))
    syncController.expectMsg(NewCheckpoint(checkpointBlock))
    result shouldEqual Right(expectedResponse)
  }

  it should "get latest block in case of blockchain re-org" in new TestSetup {
    val block = Fixtures.Blocks.ValidBlock.block
    val expectedResponse = GetLatestBlockResponse(Some(BlockInfo(block.hash, block.number)))
    (blockchain.getBestBlockNumber _)
      .expects()
      .returning(7)
    (blockchainReader.getBlockByNumber _)
      .expects(BigInt(4))
      .returning(None)
    (blockchain.getBestBlockNumber _)
      .expects()
      .returning(7)
    (blockchainReader.getBlockByNumber _)
      .expects(BigInt(4))
      .returning(Some(block))

    val result = service.getLatestBlock(GetLatestBlockRequest(4, None))

    result.runSyncUnsafe() shouldEqual Right(expectedResponse)
  }

  trait TestSetup {
    val blockchain = mock[BlockchainImpl]
    val blockchainReader = mock[BlockchainReader]
    val ledger = mock[Ledger]
    val syncController = TestProbe()
    val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator()
    val service =
      new CheckpointingService(blockchain, blockchainReader, ledger, checkpointBlockGenerator, syncController.ref)
  }
}
