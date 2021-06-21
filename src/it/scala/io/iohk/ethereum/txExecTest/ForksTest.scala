package io.iohk.ethereum.txExecTest

import java.util.concurrent.Executors
import io.iohk.ethereum.domain.{Address, BlockchainImpl, BlockchainReader, Receipt, UInt256}
import io.iohk.ethereum.ledger.{BlockExecution, BlockQueue, BlockValidation}
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, ForkBlockNumbers, MonetaryPolicyConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

// scalastyle:off magic.number
class ForksTest extends AnyFlatSpec with Matchers {

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = BlockchainConfig(
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 3,
        eip150BlockNumber = 5,
        eip160BlockNumber = 7,
        eip155BlockNumber = 0,
        eip106BlockNumber = Long.MaxValue,
        eip161BlockNumber = Long.MaxValue,
        difficultyBombPauseBlockNumber = Long.MaxValue,
        difficultyBombContinueBlockNumber = Long.MaxValue,
        difficultyBombRemovalBlockNumber = Long.MaxValue,
        byzantiumBlockNumber = Long.MaxValue,
        constantinopleBlockNumber = Long.MaxValue,
        istanbulBlockNumber = Long.MaxValue,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue,
        ecip1098BlockNumber = Long.MaxValue,
        ecip1097BlockNumber = Long.MaxValue,
        ecip1099BlockNumber = Long.MaxValue,
        ecip1049BlockNumber = None
      ),
      chainId = 0x3d.toByte,
      monetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L),
      // unused
      bootstrapNodes = Set(),
      networkId = 1,
      maxCodeSize = None,
      customGenesisFileOpt = None,
      customGenesisJsonOpt = None,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      treasuryAddress = Address(0)
    )

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
      val blockchainReader = new BlockchainReader(storages.blockHeadersStorage)
      val blockchain = BlockchainImpl(storages, blockchainReader)
      val blockValidation =
        new BlockValidation(consensus, blockchain, blockchainReader, BlockQueue(blockchain, syncConfig))
      val blockExecution =
        new BlockExecution(blockchain, blockchainReader, blockchainConfig, consensus.blockPreparator, blockValidation)
      blockExecution.executeAndValidateBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
