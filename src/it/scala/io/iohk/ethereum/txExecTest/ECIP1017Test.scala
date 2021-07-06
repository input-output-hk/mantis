package io.iohk.ethereum.txExecTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.dsl.ResultOfATypeInvocation
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ForkBlockNumbers
import io.iohk.ethereum.utils.MonetaryPolicyConfig

class ECIP1017Test extends AnyFlatSpec with Matchers {

  val EraDuration = 3

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig: BlockchainConfig = BlockchainConfig(
      monetaryPolicyConfig = MonetaryPolicyConfig(EraDuration, 0.2, 5000000000000000000L, 3000000000000000000L),
      // unused
      maxCodeSize = None,
      chainId = 0x3d.toByte,
      networkId = 1,
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 1150000,
        difficultyBombPauseBlockNumber = Long.MaxValue,
        difficultyBombContinueBlockNumber = Long.MaxValue,
        difficultyBombRemovalBlockNumber = Long.MaxValue,
        eip106BlockNumber = Long.MaxValue,
        eip150BlockNumber = 2500000,
        eip160BlockNumber = 3000000,
        eip155BlockNumber = 3000000,
        eip161BlockNumber = Long.MaxValue,
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
      customGenesisFileOpt = None,
      customGenesisJsonOpt = None,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      accountStartNonce = UInt256.Zero,
      ethCompatibleStorage = true,
      gasTieBreaker = false,
      treasuryAddress = Address(0)
    )
    val noErrors: ResultOfATypeInvocation[Right[_, Seq[Receipt]]] = a[Right[_, Seq[Receipt]]]
  }

  /** Tests the block reward calculation through out all the monetary policy through all the eras till block
    * mining reward goes to zero. Block mining reward is tested till era 200 (that starts at block number 602)
    * as the reward reaches zero at era 193 (which starts at block number 579), given an eraDuration of 3,
    * a rewardReductionRate of 0.2 and a firstEraBlockReward of 5 ether.
    */
  "Ledger" should "execute blocks with respect to block reward changed by ECIP 1017" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/ecip1017Test")

    val startBlock = 1
    val endBlock = 602

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(endBlock, fixtures)

    (startBlock to endBlock).foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchainReader = BlockchainReader(storages)
      val blockchain = BlockchainImpl(storages, blockchainReader)
      val blockValidation =
        new BlockValidation(consensus, blockchainReader, BlockQueue(blockchain, syncConfig))
      val blockExecution =
        new BlockExecution(
          blockchain,
          blockchainReader,
          testBlockchainStorages.evmCodeStorage,
          blockchainConfig,
          consensus.blockPreparator,
          blockValidation
        )
      blockExecution.executeAndValidateBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
