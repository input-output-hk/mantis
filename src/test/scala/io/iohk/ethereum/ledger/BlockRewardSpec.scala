package io.iohk.ethereum.ledger

import akka.util.ByteString

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefEmpty
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockPreparator._
import io.iohk.ethereum.ledger.BlockRewardCalculatorOps._
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.ForkBlockNumbers

class BlockRewardSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with MockFactory {

  it should "pay to the miner if no ommers included" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)
    val beforeExecutionBalance: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    afterRewardWorldState
      .getGuaranteedAccount(Address(block.header.beneficiary))
      .balance shouldEqual (beforeExecutionBalance + minerTwoOmmersReward)
  }

  // scalastyle:off magic.number
  it should "be paid to the miner even if the account doesn't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef))
    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)
    val expectedRewardAsBigInt =
      mining.blockPreparator.blockRewardCalculator.calculateMiningReward(block.header.number, 0)
    val expectedReward = UInt256(expectedRewardAsBigInt)
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual expectedReward
  }

  it should "be paid if ommers are included in block" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)

    val beforeExecutionBalance1: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    val beforeExecutionBalance2: BigInt =
      worldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance
    val beforeExecutionBalance3: BigInt =
      worldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance

    val uncleBalance1: UInt256 =
      afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance
    val uncleBalance2: UInt256 =
      afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance

    afterRewardWorldState
      .getGuaranteedAccount(Address(block.header.beneficiary))
      .balance shouldEqual (beforeExecutionBalance1 + minerTwoOmmersReward)
    uncleBalance1 shouldEqual (beforeExecutionBalance2 + ommerFiveBlocksDifferenceReward)
    uncleBalance2 shouldEqual (beforeExecutionBalance3 + ommerFiveBlocksDifferenceReward)
  }

  it should "be paid if ommers are included in block even if accounts don't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef), Seq(Address(0x1111), Address(0x2222)))
    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)
    afterRewardWorldState
      .getGuaranteedAccount(Address(block.header.beneficiary))
      .balance shouldEqual minerTwoOmmersReward
    afterRewardWorldState
      .getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary))
      .balance shouldEqual ommerFiveBlocksDifferenceReward
    afterRewardWorldState
      .getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary))
      .balance shouldEqual ommerFiveBlocksDifferenceReward
  }

  it should "be calculated correctly after byzantium fork" in new TestSetup {
    val block: Block = sampleBlockAfterByzantium(validAccountAddress)
    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)
    val address = Address(block.header.beneficiary)
    val beforeExecutionBalance: BigInt = worldState.getGuaranteedAccount(address).balance
    afterRewardWorldState
      .getGuaranteedAccount(address)
      .balance shouldEqual beforeExecutionBalance + afterByzantiumNewBlockReward
  }

  it should "be calculated correctly if ommers are included in block after byzantium fork " in new TestSetup {
    val block: Block = sampleBlockAfterByzantium(validAccountAddress4, Seq(validAccountAddress5, validAccountAddress6))

    val minerAddress = Address(block.header.beneficiary)
    val ommer1Address = Address(block.body.uncleNodesList.head.beneficiary)
    val ommer2Address = Address(block.body.uncleNodesList(1).beneficiary)

    val afterRewardWorldState: InMemoryWorldStateProxy = mining.blockPreparator.payBlockReward(block, worldState)

    val beforeExecutionBalance1: BigInt = worldState.getGuaranteedAccount(minerAddress).balance
    val beforeExecutionBalance2: BigInt = worldState.getGuaranteedAccount(ommer1Address).balance
    val beforeExecutionBalance3: BigInt = worldState.getGuaranteedAccount(ommer2Address).balance

    // spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
    val newBlockReward: BigInt = blockchainConfig.monetaryPolicyConfig.firstEraReducedBlockReward
    val ommersRewards: BigInt = (8 - (block.header.number - block.body.uncleNodesList.head.number)) * newBlockReward / 8
    val nephewRewards: BigInt = (newBlockReward / 32) * 2

    afterRewardWorldState
      .getGuaranteedAccount(minerAddress)
      .balance shouldEqual (beforeExecutionBalance1 + afterByzantiumNewBlockReward + nephewRewards)
    afterRewardWorldState
      .getGuaranteedAccount(ommer1Address)
      .balance shouldEqual (beforeExecutionBalance2 + ommersRewards)
    afterRewardWorldState
      .getGuaranteedAccount(ommer2Address)
      .balance shouldEqual (beforeExecutionBalance3 + ommersRewards)
  }

  it should "correctly distribute block reward according to ECIP1098" in new TestSetup {
    val blockNoPostTreasury = blockchainConfig.forkBlockNumbers.ecip1098BlockNumber + 1
    val blockReward = mining.blockPreparator.blockRewardCalculator.calculateMiningRewardForBlock(sampleBlockNumber)
    val blockRewardPostTreasury =
      mining.blockPreparator.blockRewardCalculator.calculateMiningRewardForBlock(blockNoPostTreasury)

    val table = Table[Boolean, BigInt, BigInt, BigInt](
      ("contract deployed", "miner reward", "treasury reward", "block no"),
      // ECIP1098 not activated
      (true, blockReward, 0, sampleBlockNumber),
      (false, blockReward, 0, sampleBlockNumber),
      // ECIP1098 previously activated but contract destroyed
      (false, blockRewardPostTreasury, 0, blockNoPostTreasury),
      // ECIP1098 activated with contract in place
      (
        true,
        MinerRewardPercentageAfterECIP1098 * blockRewardPostTreasury / 100,
        TreasuryRewardPercentageAfterECIP1098 * blockRewardPostTreasury / 100,
        blockNoPostTreasury
      )
    )

    forAll(table) { case (contractDeployed, minerReward, treasuryReward, blockNo) =>
      val minerAddress = validAccountAddress
      val block = sampleBlock(minerAddress, Nil, blockNo)
      val worldBeforeExecution =
        if (contractDeployed) worldState
        else {
          val worldWithoutTreasury = worldState.deleteAccount(treasuryAddress)
          InMemoryWorldStateProxy.persistState(worldWithoutTreasury)
        }

      val beforeExecutionMinerBalance: BigInt =
        worldBeforeExecution.getAccount(minerAddress).fold(UInt256.Zero)(_.balance)
      val beforeExecutionTreasuryBalance: BigInt =
        worldBeforeExecution.getAccount(treasuryAddress).fold(UInt256.Zero)(_.balance)

      val afterRewardWorldState: InMemoryWorldStateProxy =
        mining.blockPreparator.payBlockReward(block, worldBeforeExecution)
      val afterExecutionMinerBalance = afterRewardWorldState.getAccount(minerAddress).fold(UInt256.Zero)(_.balance)
      val afterExecutionTreasuryBalance =
        afterRewardWorldState.getAccount(treasuryAddress).fold(UInt256.Zero)(_.balance)

      afterExecutionMinerBalance shouldEqual (beforeExecutionMinerBalance + minerReward)
      afterExecutionTreasuryBalance shouldEqual (beforeExecutionTreasuryBalance + treasuryReward)
    }
  }

  // scalastyle:off magic.number
  trait TestSetup extends EphemBlockchainTestSetup {
    //+ cake overrides
    override lazy val vm: VMImpl = new MockVM()

    //- cake overrides

    val validAccountAddress: Address = Address(0xababab) // 11250603
    val validAccountAddress2: Address = Address(0xcdcdcd) // 13487565
    val validAccountAddress3: Address = Address(0xefefef) // 15724527

    val validAccountAddress4: Address = Address("0x29a2241af62c0001") // 3000000000000000001
    val validAccountAddress5: Address = Address("0x29a2241af64e2223") // 3000000000002236963
    val validAccountAddress6: Address = Address("0x29a2241af6704445") // 3000000000004473925

    val treasuryAddress = validAccountAddress2
    val baseBlockchainConfig = Config.blockchains.blockchainConfig
    private val forkBlockNumbers: ForkBlockNumbers = baseBlockchainConfig.forkBlockNumbers
    implicit override lazy val blockchainConfig: BlockchainConfig = baseBlockchainConfig
      .copy(
        treasuryAddress = treasuryAddress,
        forkBlockNumbers = forkBlockNumbers
          .copy(ecip1098BlockNumber = forkBlockNumbers.byzantiumBlockNumber + 100)
      )

    val minerTwoOmmersReward: BigInt = BigInt("5312500000000000000")
    val ommerFiveBlocksDifferenceReward: BigInt = BigInt("1875000000000000000")
    val afterByzantiumNewBlockReward: BigInt = BigInt(10).pow(18) * 3

    val worldState: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )
      .saveAccount(validAccountAddress, Account(balance = 10))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))
      .saveAccount(validAccountAddress4, Account(balance = 10))
      .saveAccount(validAccountAddress5, Account(balance = 20))
      .saveAccount(validAccountAddress6, Account(balance = 20))

    // We don't care for this tests if block is not valid
    val sampleBlockNumber = 10
    def sampleBlock(
        minerAddress: Address,
        ommerMiners: Seq[Address] = Nil,
        blockNumber: BigInt = sampleBlockNumber
    ): Block = {
      val extraFields = HefEmpty
      Block(
        header = Fixtures.Blocks.Genesis.header.copy(
          beneficiary = minerAddress.bytes,
          number = blockNumber,
          extraFields = extraFields
        ),
        body = Fixtures.Blocks.Genesis.body.copy(
          uncleNodesList = ommerMiners.map { address =>
            Fixtures.Blocks.Genesis.header.copy(beneficiary = address.bytes, number = 5)
          }
        )
      )
    }

    def sampleBlockAfterByzantium(minerAddress: Address, ommerMiners: Seq[Address] = Nil): Block = {
      val baseBlockNumber = forkBlockNumbers.byzantiumBlockNumber
      Block(
        header = Fixtures.Blocks.Genesis.header.copy(beneficiary = minerAddress.bytes, number = baseBlockNumber),
        body = Fixtures.Blocks.Genesis.body.copy(
          uncleNodesList = ommerMiners.map { address =>
            Fixtures.Blocks.Genesis.header.copy(beneficiary = address.bytes, number = baseBlockNumber + 5)
          }
        )
      )
    }
  }
}
