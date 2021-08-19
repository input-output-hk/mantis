package io.iohk.ethereum.consensus

import akka.util.ByteString

import cats.data.NonEmptyList

import monix.execution.Scheduler

import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.Consensus.BranchExecutionFailure
import io.iohk.ethereum.consensus.Consensus.ExtendedCurrentBestBranch
import io.iohk.ethereum.consensus.Consensus.ExtendedCurrentBestBranchPartially
import io.iohk.ethereum.consensus.Consensus.KeptCurrentBestBranch
import io.iohk.ethereum.consensus.Consensus.SelectedNewBestBranch
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.utils.BlockchainConfig

class ConsensusImplSpec extends AnyFlatSpec with Matchers with ScalaFutures with NormalPatience {
  import ConsensusImplSpec._
  "Consensus" should "extend the current best chain" in new ConsensusSetup {
    val chainExtension = BlockHelpers.generateChain(3, initialBestBlock)

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(chainExtension)).runToFuture) {
      _ shouldBe a[ExtendedCurrentBestBranch]
    }

    blockchainReader.getBestBlock() shouldBe Some(chainExtension.last)
  }

  it should "extends the branch partially if one block is invalid" in new ConsensusSetup {
    val chainExtension = BlockHelpers.generateChain(3, initialBestBlock)
    setFailingBlock(chainExtension(1))

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(chainExtension)).runToFuture) {
      _ shouldBe a[ExtendedCurrentBestBranchPartially]
    }
    blockchainReader.getBestBlock() shouldBe Some(chainExtension.head)
  }

  it should "keep the current best chain if the passed one is not better" in new ConsensusSetup {
    val chainWithLowWeight =
      BlockHelpers.generateChain(3, initialChain(2), b => b.copy(header = b.header.copy(difficulty = 1)))

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(chainWithLowWeight)).runToFuture) {
      _ shouldBe KeptCurrentBestBranch
    }
    blockchainReader.getBestBlock() shouldBe Some(initialBestBlock)
  }

  it should "reorganise the chain if the new chain is better" in new ConsensusSetup {
    val newBetterBranch =
      BlockHelpers.generateChain(3, initialChain(2), b => b.copy(header = b.header.copy(difficulty = 10000000)))

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(newBetterBranch)).runToFuture) {
      _ shouldBe a[SelectedNewBestBranch]
    }
    blockchainReader.getBestBlock() shouldBe Some(newBetterBranch.last)
  }

  it should "return an error a block execution is failing during a reorganisation" in new ConsensusSetup {
    val newBetterBranch =
      BlockHelpers.generateChain(3, initialChain(2), b => b.copy(header = b.header.copy(difficulty = 10000000)))

    // only first block will execute
    setFailingBlock(newBetterBranch(1))

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(newBetterBranch)).runToFuture) {
      _ shouldBe a[BranchExecutionFailure]
    }
    blockchainReader.getBestBlock() shouldBe Some(initialBestBlock)
  }
}

object ConsensusImplSpec {
  val initialChain: List[Block] = BlockHelpers.genesis +: BlockHelpers.generateChain(4, BlockHelpers.genesis)
  val initialBestBlock = initialChain.last

  abstract class ConsensusSetup {
    private val testSetup = new EphemBlockchainTestSetup with MockFactory {
      override lazy val blockExecution: BlockExecution = stub[BlockExecution]
      (blockExecution
        .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
        .when(*, *, *)
        .anyNumberOfTimes()
        .onCall { (blocks, _, _) =>
          val executedBlocks = blocks
            .takeWhile(b => !failingBlockHash.contains(b.hash))
            .map(b => BlockData(b, Nil, ChainWeight.zero))
          executedBlocks.foreach(b => blockchainWriter.save(b.block, b.receipts, b.weight, false))
          (
            executedBlocks,
            blocks.find(b => failingBlockHash.contains(b.hash)).map(_ => ValidationAfterExecError("test error"))
          )
        }
    }

    initialChain.foldLeft(ChainWeight.zero) { (previousWeight, block) =>
      val weight = previousWeight.increase(block.header)
      testSetup.blockchainWriter.save(block, Nil, weight, saveAsBestBlock = true)
      weight
    }

    private var failingBlockHash: Option[ByteString] = None

    val consensus = testSetup.consensus
    val blockchainReader = testSetup.blockchainReader
    implicit val scheduler: Scheduler = Scheduler.global
    implicit val blockchainConfig: BlockchainConfig = testSetup.blockchainConfig

    def setFailingBlock(block: Block): Unit = failingBlockHash = Some(block.hash)

  }
}
