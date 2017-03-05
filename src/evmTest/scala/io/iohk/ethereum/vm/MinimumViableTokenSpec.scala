package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class MinimumViableTokenSpec extends FreeSpec with Matchers {

  "EVM running MinimumViableToken contract" - {

    "should init the balance and allow token transfer" in new EvmTestEnv {
      val sender = createAccount(balance = 10)
      val receiver = createAccount(balance = 10)

      val (_, contract) = deployContract("MinimumViableToken", creatorAddress = sender, constructorArgs = Seq(150))

      val senderBalanceBeforeRes = contract.balanceOf(sender).call()
      senderBalanceBeforeRes.returnData shouldBe DataWord(150).bytes

      val transferRes = contract.transfer(receiver, 40).call(sender = sender)
      transferRes.error shouldBe None
      transferRes.gasUsed shouldBe 26770

      val receiverBalanceAfterRes = contract.balanceOf(receiver).call()
      receiverBalanceAfterRes.returnData shouldBe DataWord(40).bytes

      val senderBalanceAfterRes = contract.balanceOf(sender).call()
      senderBalanceAfterRes.returnData shouldBe DataWord(110).bytes
      senderBalanceAfterRes.gasUsed shouldBe 481 // TODO: why 481? should be 0 as it's read only
    }

    "should return an error when attempted to transfer more tokens than owned" in new EvmTestEnv {
      val sender = createAccount(balance = 10)
      val receiver = createAccount(balance = 10)

      val (_, contract) = deployContract("MinimumViableToken", creatorAddress = sender, constructorArgs = Seq(100))

      val transferRes = contract.transfer(receiver, 200).call(sender = sender)
      transferRes.error shouldBe Some(StackOverflow) // TODO: why StackOverflow?
    }

    "should return an error when attempted to deploy and run out of gas" in new EvmTestEnv {
      val sender = createAccount(balance = 10)
      val receiver = createAccount(balance = 10)

      val (result, _) = deployContract("MinimumViableToken", creatorAddress = sender,
        constructorArgs = Seq(100), gasLimit = 10)

      result.error shouldBe Some(OutOfGas)
    }

    "should return an error when attempted to transfer and run out of gas" in new EvmTestEnv {
      val sender = createAccount(balance = 10)
      val receiver = createAccount(balance = 10)

      val (_, contract) = deployContract("MinimumViableToken", creatorAddress = sender, constructorArgs = Seq(100))

      val transferRes = contract.transfer(receiver, 10).call(sender = sender, gasLimit = 1234)
      transferRes.error shouldBe Some(OutOfGas)
    }
  }

}
