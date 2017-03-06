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

      contract.balanceOf(sender).call().returnData shouldBe DataWord(150).bytes

      val transferRes = contract.transfer(receiver, 40).call(sender = sender)
      transferRes.error shouldBe None
      transferRes.gasUsed shouldBe 26770

      contract.balanceOf(receiver).call().returnData shouldBe DataWord(40).bytes
      contract.balanceOf(sender).call().returnData shouldBe DataWord(110).bytes
    }

    "should return an error when attempted to transfer more tokens than owned" in new EvmTestEnv {
      val sender = createAccount(balance = 10)
      val receiver = createAccount(balance = 10)

      val (_, contract) = deployContract("MinimumViableToken", creatorAddress = sender, constructorArgs = Seq(100))

      val transferRes = contract.transfer(receiver, 200).call(sender = sender)
      transferRes.error shouldBe Some(InvalidOpCode)
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
