package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.vm.MockWorldState._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

// scalastyle:off object.name
class CallOpcodesSpecPostEip161 extends WordSpec with Matchers with PropertyChecks {

  val config = EvmConfig.PostEIP161ConfigBuilder(None)
  val startState = MockWorldState(touchedAccounts = Set.empty, noEmptyAccountsCond = true)
  import config.feeSchedule._

  val fxt = new CallOpFixture(config, startState)

  "CALL" when {

    "call depth limit is reached" should {
      val context: PC = fxt.context.copy(env = fxt.env.copy(callDepth = EvmConfig.MaxCallDepth))
      val call = fxt.CallResult(op = CALL, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }
    }

    "call value is greater than balance" should {

      val call = fxt.CallResult(op = CALL, value = fxt.initialBalance + 1)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }
    }

    "external contract terminates abnormally" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.CallResult(op = CALL, context)

      "modify only touched accounts in world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram.touchAccounts(fxt.ownerAddr, fxt.extAddr)
      }
    }

    "calling an empty" should {

      val contextEmptyAccount: PC = fxt.context.copy(world = fxt.worldWithExtEmptyAccount)
      val callEmptyAccount = fxt.CallResult(op = CALL, contextEmptyAccount)
      val callZeroTransfer = fxt.CallResult(op = CALL, contextEmptyAccount, value = UInt256.Zero)

      "consume correct gas (refund call gas, add new account modifier) when transferring value to Empty Account" in {
        val expectedGas = G_call + G_callvalue + G_newaccount - G_callstipend + fxt.expectedMemCost
        callEmptyAccount.stateOut.gasUsed shouldEqual expectedGas
        callEmptyAccount.world.touchedAccounts.size shouldEqual 2
      }

      "consume correct gas when transferring no value to Empty Account" in {
        val expectedGas = G_call + fxt.expectedMemCost
        callZeroTransfer.stateOut.gasUsed shouldEqual expectedGas
        callZeroTransfer.world.touchedAccounts.size shouldEqual 2
      }
    }
  }
}
