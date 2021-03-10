package io.iohk.ethereum.txExecTest

import java.util.concurrent.Executors

import io.iohk.ethereum.domain.{Address, BlockchainImpl, Receipt, UInt256}
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, MonetaryPolicyConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

class ECIP1017Test extends AnyFlatSpec with Matchers {

  val EraDuration = 3

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = BlockchainConfig(
      monetaryPolicyConfig = MonetaryPolicyConfig(EraDuration, 0.2, 5000000000000000000L, 3000000000000000000L),
      // unused
      maxCodeSize = None,
      chainId = 0x3d.toByte,
      networkId = 1,
      protocolVersion = 63,
      frontierBlockNumber = 0,
      homesteadBlockNumber = 1150000,
      eip106BlockNumber = Long.MaxValue,
      eip150BlockNumber = 2500000,
      eip160BlockNumber = 3000000,
      eip155BlockNumber = 3000000,
      eip161BlockNumber = Long.MaxValue,
      byzantiumBlockNumber = Long.MaxValue,
      constantinopleBlockNumber = Long.MaxValue,
      istanbulBlockNumber = Long.MaxValue,
      customGenesisFileOpt = None,
      daoForkConfig = None,
      difficultyBombPauseBlockNumber = Long.MaxValue,
      difficultyBombContinueBlockNumber = Long.MaxValue,
      difficultyBombRemovalBlockNumber = Long.MaxValue,
      bootstrapNodes = Set(),
      accountStartNonce = UInt256.Zero,
      ethCompatibleStorage = true,
      gasTieBreaker = false,
      atlantisBlockNumber = Long.MaxValue,
      aghartaBlockNumber = Long.MaxValue,
      phoenixBlockNumber = Long.MaxValue,
      petersburgBlockNumber = Long.MaxValue,
      ecip1098BlockNumber = Long.MaxValue,
      treasuryAddress = Address(0),
      ecip1097BlockNumber = Long.MaxValue,
      ecip1099BlockNumber = Long.MaxValue
    )
    val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

    val noErrors = a[Right[_, Seq[Receipt]]]
  }

  val vm = new Ledger.VMImpl

  /**
    * Tests the block reward calculation through out all the monetary policy through all the eras till block
    * mining reward goes to zero. Block mining reward is tested till era 200 (that starts at block number 602)
    * as the reward reaches zero at era 193 (which starts at block number 579), given an eraDuration of 3,
    * a rewardReductionRate of 0.2 and a firstEraBlockReward of 5 ether.
    */
  "Ledger" should "execute blocks with respect to block reward changed by ECIP 1017" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/ecip1017Test")

    val startBlock = 1
    val endBlock = 602

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(startBlock, fixtures)

    (startBlock to endBlock) foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchain = BlockchainImpl(storages)
      val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
      val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
      blockExecution.executeAndValidateBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
