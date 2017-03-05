package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class ContractCallingItselfSpec extends FreeSpec with Matchers {

  "EVM running ContractCallingItself contract" - {

    "should handle a call to itself" in new EvmTestEnv {
      val (_, contract) = deployContract("ContractCallingItself")

      contract.getSomeVar().call().returnData shouldBe DataWord(10).bytes

      val result = contract.doubleSomeVar().call()
      result.error shouldBe None

      contract.getSomeVar().call().returnData shouldBe DataWord(20).bytes
    }
  }

}
