package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class ThrowSpec extends AnyFreeSpec with Matchers {

  "EVM running Throw contract" - {

    "should handle throwing" in new EvmTestEnv {
      val (_, contract) = deployContract("Throw")

      val result = contract.justThrow().call()

      result.error shouldBe Some(InvalidOpCode(0xfd.toByte))
    }
  }

}
