package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FreeSpec, Matchers}

class CallSelfDestructSpec extends FreeSpec with Matchers {

  "EVM running CallSelfDestruct contract" - {

    "should refund only once" in new EvmTestEnv {
      val (_, callSelfDestruct) = deployContract("CallSelfDestruct")

      val callRes = callSelfDestruct.callDestruct().call()
      callRes.error shouldBe None

      callRes.gasRefund shouldBe 24000
    }
  }

}
