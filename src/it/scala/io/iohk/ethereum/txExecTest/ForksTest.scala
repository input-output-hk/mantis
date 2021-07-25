package io.iohk.ethereum.txExecTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.dsl.ResultOfATypeInvocation
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ForkBlockNumbers
import io.iohk.ethereum.utils.MonetaryPolicyConfig

class ForksTest extends AnyFlatSpec with Matchers {

  trait TestSetup extends ScenarioSetup {
    implicit override lazy val blockchainConfig: BlockchainConfig = BlockchainConfig(
      forkBlockNumbers = ForkBlockNumbers.Empty.copy(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 3,
        eip150BlockNumber = 5,
        eip160BlockNumber = 7,
        eip155BlockNumber = 0
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
    val noErrors: ResultOfATypeInvocation[Right[_, Seq[Receipt]]] = a[Right[_, Seq[Receipt]]]
  }

  "Ledger" should "execute blocks with respect to forks" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/forksTest")

    val startBlock = 1
    val endBlock = 11

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(endBlock, fixtures)

    (startBlock to endBlock).foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchainMetadata = getNewBlockchainMetadata
      val blockchainReader = BlockchainReader(storages, blockchainMetadata)
      val blockchainWriter = BlockchainWriter(storages, blockchainMetadata)
      val blockchain = BlockchainImpl(storages, blockchainReader, blockchainMetadata)
      val blockValidation =
        new BlockValidation(mining, blockchainReader, BlockQueue(blockchain, blockchainReader, syncConfig))
      val blockExecution =
        new BlockExecution(
          blockchain,
          blockchainReader,
          blockchainWriter,
          testBlockchainStorages.evmCodeStorage,
          mining.blockPreparator,
          blockValidation
        )
      blockExecution.executeAndValidateBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
