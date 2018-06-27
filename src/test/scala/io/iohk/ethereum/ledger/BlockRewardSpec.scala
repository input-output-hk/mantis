package io.iohk.ethereum.ledger

import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.ledger.Ledger.VMImpl
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class BlockRewardSpec extends FlatSpec with Matchers with MockFactory {

  val blockchainConfig = BlockchainConfig(Config.config)
  val syncConfig = SyncConfig(Config.config)

  val blockchain: BlockchainImpl = mock[BlockchainImpl]

  "Reward Calculation" should "pay to the miner if no ommers included" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)
    val beforeExecutionBalance: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual (beforeExecutionBalance + minerTwoOmmersReward)
  }

  // scalastyle:off magic.number
  "Reward" should "be paid to the miner even if the account doesn't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef))
    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)
    val expectedReward = UInt256(ledger.blockRewardCalculator.calcBlockMinerReward(block.header.number, 0))
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual expectedReward
  }

  "Reward Calculation" should "be paid if ommers are included in block" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)
    val beforeExecutionBalance1: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    val beforeExecutionBalance2: BigInt = worldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance
    val beforeExecutionBalance3: BigInt = worldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual (beforeExecutionBalance1 + minerTwoOmmersReward)
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance shouldEqual (beforeExecutionBalance2 + ommerFiveBlocksDifferenceReward)
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance shouldEqual (beforeExecutionBalance3 + ommerFiveBlocksDifferenceReward)
  }

  "Reward" should "be paid if ommers are included in block even if accounts don't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef), Seq(Address(0x1111), Address(0x2222)))
    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual minerTwoOmmersReward
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance shouldEqual ommerFiveBlocksDifferenceReward
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance shouldEqual ommerFiveBlocksDifferenceReward
  }

  "Reward Calculation" should "be calculated correctly after byzantium fork" in new TestSetup {
    val block: Block = sampleBlockAfterByzantium(validAccountAddress)
    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)
    val address = Address(block.header.beneficiary)
    val beforeExecutionBalance: BigInt = worldState.getGuaranteedAccount(address).balance
    afterRewardWorldState.getGuaranteedAccount(address).balance shouldEqual beforeExecutionBalance + afterByzantiumNewBlockReward
  }

  "Reward Calculation" should "be calculated correctly if ommers are included in block after byzantium fork " in new TestSetup {
    val block: Block = sampleBlockAfterByzantium(validAccountAddress4, Seq(validAccountAddress5, validAccountAddress6))

    val minerAddress = Address(block.header.beneficiary)
    val ommer1Address = Address(block.body.uncleNodesList.head.beneficiary)
    val ommer2Address = Address(block.body.uncleNodesList(1).beneficiary)

    val afterRewardWorldState: InMemoryWorldStateProxy = ledger.payBlockReward(block, worldState)

    val beforeExecutionBalance1: BigInt = worldState.getGuaranteedAccount(minerAddress).balance
    val beforeExecutionBalance2: BigInt = worldState.getGuaranteedAccount(ommer1Address).balance
    val beforeExecutionBalance3: BigInt = worldState.getGuaranteedAccount(ommer2Address).balance

    // spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
    val newBlockReward: BigInt = blockchainConfig.monetaryPolicyConfig.firstEraReducedBlockReward
    val ommersRewards: BigInt = (8 - (block.header.number - block.body.uncleNodesList.head.number)) * newBlockReward / 8
    val nephewRewards: BigInt = newBlockReward / 2 / 32

    afterRewardWorldState.getGuaranteedAccount(minerAddress).balance shouldEqual (beforeExecutionBalance1 + afterByzantiumNewBlockReward + nephewRewards)
    afterRewardWorldState.getGuaranteedAccount(ommer1Address).balance shouldEqual (beforeExecutionBalance2 + ommersRewards)
    afterRewardWorldState.getGuaranteedAccount(ommer2Address).balance shouldEqual (beforeExecutionBalance3 + ommersRewards)
  }


  // scalastyle:off magic.number
  trait TestSetup extends EphemBlockchainTestSetup {
    //+ cake overrides
    override lazy val vm: VMImpl = new MockVM()

    // Just make the type a bit more specific, since this is needed by the test cases
    override lazy val ledger: LedgerImpl = newLedger()
    //- cake overrides


    val validAccountAddress = Address(0xababab)  // 11250603
    val validAccountAddress2 = Address(0xcdcdcd) // 13487565
    val validAccountAddress3 = Address(0xefefef) // 15724527

    val validAccountAddress4 = Address("0x29a2241af62c0001") // 3000000000000000001
    val validAccountAddress5 = Address("0x29a2241af64e2223") // 3000000000002236963
    val validAccountAddress6 = Address("0x29a2241af6704445") // 3000000000004473925

    val minerTwoOmmersReward = BigInt("5312500000000000000")
    val ommerFiveBlocksDifferenceReward = BigInt("1875000000000000000")
    val afterByzantiumNewBlockReward: BigInt = BigInt(10).pow(18) * 3

    val worldState: InMemoryWorldStateProxy = BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None,
      noEmptyAccounts = false, ethCompatibleStorage = true)
      .saveAccount(validAccountAddress, Account(balance = 10))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))
      .saveAccount(validAccountAddress4, Account(balance = 10))
      .saveAccount(validAccountAddress5, Account(balance = 20))
      .saveAccount(validAccountAddress6, Account(balance = 20))

    // We don't care for this tests if block is not valid
    def sampleBlock(minerAddress: Address, ommerMiners: Seq[Address] = Nil): Block = {
      Block(
        header = Fixtures.Blocks.Genesis.header.copy(beneficiary = minerAddress.bytes, number = 10),
        body = Fixtures.Blocks.Genesis.body.copy(
          uncleNodesList = ommerMiners.map{ address =>
            Fixtures.Blocks.Genesis.header.copy(beneficiary = address.bytes, number = 5)
          }
        )
      )
    }

    def sampleBlockAfterByzantium(minerAddress: Address, ommerMiners: Seq[Address] = Nil): Block = {
      val baseBlockNumber = blockchainConfig.byzantiumBlockNumber
      Block(
        header = Fixtures.Blocks.Genesis.header.copy(beneficiary = minerAddress.bytes, number = baseBlockNumber),
        body = Fixtures.Blocks.Genesis.body.copy(
          uncleNodesList = ommerMiners.map{ address =>
            Fixtures.Blocks.Genesis.header.copy(beneficiary = address.bytes, number = baseBlockNumber + 5)
          }
        )
      )
    }
  }
}
