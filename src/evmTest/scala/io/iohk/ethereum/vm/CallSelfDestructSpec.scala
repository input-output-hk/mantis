package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec

class CallSelfDestructSpec extends AnyFreeSpec with Matchers {

  "EVM running CallSelfDestruct contract" - {

    "should refund only once" in new EvmTestEnv {
      val (_, callSelfDestruct) = deployContract("CallSelfDestruct")

      val callRes = callSelfDestruct.callDestruct().call()

      callRes.error shouldBe None
      callRes.gasRefund shouldBe 24000
    }
  }

}
