package io.iohk.ethereum.ledger

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Account, Address, Block}
import io.iohk.ethereum.utils.Config
import org.scalatest.{FlatSpec, Matchers}

class BlockRewardSpec extends FlatSpec with Matchers {

  "Reward Calculation" should "pay to the miner if no ommers included" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = Ledger.payBlockReward(Config.Blockchain.BlockReward, block, worldState)
    val beforeExecutionBalance: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual (beforeExecutionBalance + minerTwoOmmersReward)
  }

  "Reward" should "be paid to the miner even if the account doesn't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef))
    val afterRewardWorldState: InMemoryWorldStateProxy = Ledger.payBlockReward(Config.Blockchain.BlockReward, block, worldState)
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual Config.Blockchain.BlockReward
  }

  "Reward Calculation" should "be paid if ommers are included in block" in new TestSetup {
    val block = sampleBlock(validAccountAddress, Seq(validAccountAddress2, validAccountAddress3))
    val afterRewardWorldState: InMemoryWorldStateProxy = Ledger.payBlockReward(Config.Blockchain.BlockReward, block, worldState)
    val beforeExecutionBalance1: BigInt = worldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance
    val beforeExecutionBalance2: BigInt = worldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance
    val beforeExecutionBalance3: BigInt = worldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual (beforeExecutionBalance1 + minerTwoOmmersReward)
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance shouldEqual (beforeExecutionBalance2 + ommerFiveBlocksDifferenceReward)
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance shouldEqual (beforeExecutionBalance3 + ommerFiveBlocksDifferenceReward)
  }

  "Reward" should "be paid if ommers are included in block even if accounts don't exist" in new TestSetup {
    val block = sampleBlock(Address(0xdeadbeef), Seq(Address(0x1111), Address(0x2222)))
    val afterRewardWorldState: InMemoryWorldStateProxy = Ledger.payBlockReward(Config.Blockchain.BlockReward, block, worldState)
    afterRewardWorldState.getGuaranteedAccount(Address(block.header.beneficiary)).balance shouldEqual minerTwoOmmersReward
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList.head.beneficiary)).balance shouldEqual ommerFiveBlocksDifferenceReward
    afterRewardWorldState.getGuaranteedAccount(Address(block.body.uncleNodesList(1).beneficiary)).balance shouldEqual ommerFiveBlocksDifferenceReward
  }


  trait TestSetup {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val validAccountAddress = Address(0xababab)
    val validAccountAddress2 = Address(0xcdcdcd)
    val validAccountAddress3 = Address(0xefefef)

    val minerTwoOmmersReward = BigInt("5312500000000000000")
    val ommerFiveBlocksDifferenceReward = BigInt("1875000000000000000")

    val worldState: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    ).saveAccount(validAccountAddress, Account(balance = 10))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))

    // We don't care for this tests if block is not valid
    def sampleBlock(minerAddress: Address, ommerMiners: Seq[Address] = Seq.empty): Block = {
      Block(
        header = Fixtures.Blocks.Genesis.header.copy(beneficiary = minerAddress.bytes, number = 10),
        body = Fixtures.Blocks.Genesis.body.copy(
          uncleNodesList = ommerMiners.map(a => Fixtures.Blocks.Genesis.header.copy(beneficiary = a.bytes, number = 5))
        )
      )
    }
  }
}
