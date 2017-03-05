package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class CallerSpec extends FreeSpec with Matchers {

  "EVM running Caller contract" - {

    "should handle a call to Callee" in new EvmTestEnv {
      val (_, callee) = deployContract("Callee")
      val (_, caller) = deployContract("Caller")

      val callRes = caller.makeACall(callee.address, 123).call()
      callRes.error shouldBe None

      callee.getFoo().call().returnData shouldBe DataWord(123).bytes
    }
  }

}
