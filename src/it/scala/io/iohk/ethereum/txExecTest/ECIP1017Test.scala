package io.iohk.ethereum.txExecTest

import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockchainImpl, Receipt, UInt256}
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM
import org.scalatest.{FlatSpec, Matchers}

class ECIP1017Test extends FlatSpec with Matchers {

  val EraDuration = 3

  val blockchainConfig = new BlockchainConfig {
    override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(EraDuration, 0.2, 5000000000000000000L)

    // unused
    override val chainId: Byte = 0x3d
    override val frontierBlockNumber: BigInt = 0
    override val homesteadBlockNumber: BigInt = 1150000
    override val eip150BlockNumber: BigInt = 2500000
    override val eip160BlockNumber: BigInt = 3000000
    override val eip155BlockNumber: BigInt = 3000000
    override val eip106BlockNumber: BigInt = Long.MaxValue
    override val customGenesisFileOpt: Option[String] = None
    override val daoForkConfig: Option[DaoForkConfig] = None
    override val difficultyBombPauseBlockNumber: BigInt = Long.MaxValue
    override val difficultyBombContinueBlockNumber: BigInt = Long.MaxValue
    override val accountStartNonce: UInt256 = UInt256.Zero
  }

  val noErrors = a[Right[_, Seq[Receipt]]]

  val validators = new Validators {
    val blockValidator: BlockValidator = BlockValidator
    val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
    val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
    val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
  }

  /**
    * Tests the block reward calculation through out all the monetary policy through all the eras till block
    * mining reward goes to zero. Block mining reward is tested till era 200 (that starts at block number 602)
    * as the reward reaches zero at era 193 (which starts at block number 579), given an eraDuration of 3.
    */
  "Ledger" should "execute blocks with respect to block reward changed by ECIP 1017" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/ecip1017Test")

    val startBlock = 1
    val endBlock = 602

    (startBlock to endBlock) foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchain = BlockchainImpl(storages)
      val ledger = new LedgerImpl(VM, blockchain, blockchainConfig)

      ledger.executeBlock(fixtures.blockByNumber(blockToExecute), validators) shouldBe noErrors
    }
  }

}
