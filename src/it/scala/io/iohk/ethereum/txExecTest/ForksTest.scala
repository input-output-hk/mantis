package io.iohk.ethereum.txExecTest

import java.util.concurrent.Executors

import io.iohk.ethereum.domain.{ BlockchainImpl, Receipt, UInt256 }
import io.iohk.ethereum.ledger.{ BlockExecution, BlockQueue, BlockValidation }
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{ BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig }
import org.scalatest.{ FlatSpec, Matchers }

import scala.concurrent.ExecutionContext

// scalastyle:off magic.number
class ForksTest extends FlatSpec with Matchers {

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 3
      override val eip150BlockNumber: BigInt = 5
      override val eip160BlockNumber: BigInt = 7
      override val eip155BlockNumber: BigInt = 0
      override val eip106BlockNumber: BigInt = Long.MaxValue
      override val chainId: Byte = 0x3d.toByte
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L)

      // unused
      override val bootstrapNodes: Set[String] = Set()
      override val networkId: Int = 1
      override val maxCodeSize: Option[BigInt] = None
      override val eip161BlockNumber: BigInt = Long.MaxValue
      override val customGenesisFileOpt: Option[String] = None
      override val difficultyBombPauseBlockNumber: BigInt = Long.MaxValue
      override val difficultyBombContinueBlockNumber: BigInt = Long.MaxValue
      override val difficultyBombRemovalBlockNumber: BigInt = Long.MaxValue
      override val byzantiumBlockNumber: BigInt = Long.MaxValue
      override val constantinopleBlockNumber: BigInt = Long.MaxValue
      override val accountStartNonce: UInt256 = UInt256.Zero
      override val daoForkConfig: Option[DaoForkConfig] = None
      override val gasTieBreaker: Boolean = false
      override val ethCompatibleStorage: Boolean = true
      override val atlantisBlockNumber: BigInt = Long.MaxValue
      override val aghartaBlockNumber: BigInt = Long.MaxValue
      override val phoenixBlockNumber: BigInt = Long.MaxValue
    }

    val noErrors = a[Right[_, Seq[Receipt]]]
    val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  }

  "Ledger" should "execute blocks with respect to forks" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/forksTest")

    val startBlock = 1
    val endBlock = 11

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(startBlock, fixtures)

    (startBlock to endBlock) foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchain = BlockchainImpl(storages)
      val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
      val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
      blockExecution.executeBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
