package io.iohk.ethereum.consensus

import cats.data.NonEmptyList
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.Consensus.{ExtendedCurrentBestBranch, KeptCurrentBestBranch, SelectedNewBestBranch}
import io.iohk.ethereum.domain.{Block, ChainWeight}
import io.iohk.ethereum.ledger.{BlockData, BlockExecution}
import io.iohk.ethereum.utils.BlockchainConfig
import monix.execution.Scheduler
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ScheduledExecutorService

class ConsensusImplSpec extends AnyFlatSpec with Matchers with ScalaFutures {
  import ConsensusImplSpec._
  "Consensus" should "extend the current best chain" in new ConsensusSetup {
    val chainExtension = BlockHelpers.generateChain(3, initialBestBlock)

    whenReady(consensus.evaluateBranch(NonEmptyList.fromListUnsafe(chainExtension)).runToFuture) {
      _ shouldBe a[ExtendedCurrentBestBranch]
    }

    blockchainReader.getBestBlockNumber() shouldBe chainExtension.last.number
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
    blockchainReader.getBestBlockNumber() shouldBe newBetterBranch.last.number
  }
}

object ConsensusImplSpec {
  val initialChain = BlockHelpers.genesis +: BlockHelpers.generateChain(4, BlockHelpers.genesis)
  val initialBestBlock = initialChain.last

  abstract class ConsensusSetup {
    private val testSetup = new EphemBlockchainTestSetup with MockFactory {
      override lazy val blockExecution: BlockExecution = stub[BlockExecution]
      (blockExecution
        .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
        .when(*, *, *)
        .anyNumberOfTimes()
        .onCall((blocks, _, _) => (blocks.map(b => BlockData(b, Nil, ChainWeight.zero)), None))
    }

    initialChain.foldLeft(ChainWeight.zero) { (previousWeight, block) =>
      val weight = previousWeight.increase(block.header)
      testSetup.blockchainWriter.save(block, Nil, weight, saveAsBestBlock = true)
      weight
    }

    val consensus = testSetup.consensus
    val blockchainReader = testSetup.blockchainReader
    implicit val scheduler: Scheduler = Scheduler.global
    implicit val blockchainConfig: BlockchainConfig = testSetup.blockchainConfig

  }
}
