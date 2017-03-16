package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class MutualRecursionSpec extends FreeSpec with Matchers {

  "EVM running MutualRecursion contract" - {

    "should handle a call to mutually recursive functions" in new EvmTestEnv {
      val (_, contract) = deployContract("MutualRecursion")

      val isOddRes = contract.isOdd(9).call()

      isOddRes.error shouldBe None
      isOddRes.returnData shouldBe UInt256(true).bytes

      val isEvenRes = contract.isEven(99).call()
      isEvenRes.error shouldBe None
      isEvenRes.returnData shouldBe UInt256(false).bytes
    }
  }

}
