package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class FibonacciSpec extends FreeSpec with Matchers {

  "EVM running Fibonacci contract" - {

    "should handle getNewFib call" in new EvmTestEnv {
      val (_, contract) = deployContract("Fibonacci")

      val result = contract.getNewFib(5).call()

      result.error shouldBe None
      result.returnData shouldBe UInt256(5).bytes
    }

    "should allow storage write/read" in new EvmTestEnv {
      val (_, contract) = deployContract("Fibonacci")

      val getNewRes = contract.getNewFib(6).call()

      getNewRes.error shouldBe None
      contract.storage.load(UInt256(0)) shouldBe UInt256(8)

      val getStoredRes = contract.getStoredFib().call()

      getStoredRes.error shouldBe None
      getStoredRes.returnData shouldBe UInt256(8).bytes
    }
  }

}
