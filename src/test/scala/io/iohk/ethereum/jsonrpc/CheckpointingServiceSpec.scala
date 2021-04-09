package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.domain.{Block, BlockBody, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.CheckpointingService._
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

//      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchain.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
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

    val previousCheckpoint = Fixtures.Blocks.ValidBlock.block
    val hash = previousCheckpoint.hash

    forAll(nums) { case (k, m, n) =>
      val checkpointedBlockNum: BigInt = k * m
      val bestBlockNum: BigInt = checkpointedBlockNum + n

      val block = Block(Fixtures.Blocks.ValidBlock.header.copy(number = checkpointedBlockNum), BlockBody.empty)

      val request = GetLatestBlockRequest(k, Some(hash))
      val expectedResponse = GetLatestBlockResponse(Some(BlockInfo(block.hash, block.number)))

//      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchain.getBlockHeaderByHash _).expects(hash).returning(Some(previousCheckpoint.header))
      (blockchain.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
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

//      (blockchain.getBestBlockNumber _).expects().returning(bestBlockNum)
      (blockchain.getBlockHeaderByHash _).expects(hash).returning(None)
      (blockchain.getBlockByNumber _).expects(checkpointedBlockNum).returning(Some(block))
      val result = service.getLatestBlock(request)

      result.runSyncUnsafe() shouldEqual Right(expectedResponse)
    }
  }

  it should "send new checkpoint to Sync" in new TestSetup {
    val hash = Fixtures.Blocks.ValidBlock.block.hash
    val signatures = Nil
    val request = PushCheckpointRequest(hash, signatures)
    val expectedResponse = PushCheckpointResponse()

    val result = service.pushCheckpoint(request).runSyncUnsafe()

    syncController.expectMsg(NewCheckpoint(hash, signatures))
    result shouldEqual Right(expectedResponse)
  }

  trait TestSetup {
    val blockchain = mock[BlockchainImpl]
    val syncController = TestProbe()
    val service = new CheckpointingService(blockchain, syncController.ref)
  }
}
