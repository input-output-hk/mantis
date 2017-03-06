package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class ThrowSpec extends FreeSpec with Matchers {

  "EVM running Throw contract" - {

    "should handle throwing" in new EvmTestEnv {
      val (_, contract) = deployContract("Throw")

      val result = contract.justThrow().call()

      result.error shouldBe Some(InvalidOpCode)
    }
  }

}
