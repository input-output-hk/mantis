package io.iohk.ethereum.vm

import akka.util.ByteString

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.vm.MockWorldState._

import Fixtures.blockchainConfig

class CallOpcodesPostBerlin(val config: EvmConfig = EvmConfig.BerlinConfigBuilder(blockchainConfig))
    extends CallOpcodesPostEip2929Spec(config) {
  override val fxt = new Eip2929CallOpFixture(config, Fixtures.BerlinBlockNumber)
}

class CallOpcodesPostMagneto(val config: EvmConfig = EvmConfig.MagnetoConfigBuilder(blockchainConfig))
    extends CallOpcodesPostEip2929Spec(config) {
  override val fxt = new Eip2929CallOpFixture(config, Fixtures.MagnetoBlockNumber)
}

abstract class CallOpcodesPostEip2929Spec(config: EvmConfig)
    extends AnyWordSpec
    with CallOpCodesBehaviors
    with Matchers
    with ScalaCheckPropertyChecks {

  import config.feeSchedule._

  protected[this] val fxt: CallOpFixture

  "CALL" when {

    "external contract terminates normally" should {
      val call = fxt.ExecuteCall(op = CALL)

      behave.like(callNormalTermination(fxt, call))

      "consume correct gas (refund unused gas) (cold access)" in {
        val call = fxt.ExecuteCall(op = CALL)
        val expectedGas = fxt.requiredGas - G_callstipend + G_cold_account_access + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund unused gas) (warm access)" in {
        val call = fxt.ExecuteCall(op = CALL, toAlreadyAccessed = true)
        val expectedGas = fxt.requiredGas - G_callstipend + G_warm_storage_read + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call depth limit is reached" should {
      val call = fxt.ExecuteCall(op = CALL, context = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth))
      behave.like(callDepthLimitReached(fxt, call))

      "consume correct gas (refund call gas) (cold access)" in {
        val expectedGas = G_cold_account_access + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
        //if a scope reverts, the access lists should be in the state they were in before that scope was entered
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm access)" in {
        val call =
          fxt.ExecuteCall(
            op = CALL,
            toAlreadyAccessed = true,
            context = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
          )
        val expectedGas = G_warm_storage_read + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call value is greater than balance" should {

      val call = fxt.ExecuteCall(op = CALL, value = fxt.initialBalance + 1)
      behave.like(callValueGreaterThanBalance(fxt, call))

      "consume correct gas (refund call gas) (cold access)" in {
        val expectedGas = G_cold_account_access + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm access)" in {
        val call = fxt.ExecuteCall(op = CALL, toAlreadyAccessed = true, value = fxt.initialBalance + 1)
        val expectedGas = G_warm_storage_read + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call value is zero" should {
      "adjust gas cost (cold access)" in {
        val call = fxt.ExecuteCall(op = CALL, value = 0)
        val expectedGas = fxt.requiredGas + G_cold_account_access + fxt.expectedMemCost - (G_sset - G_sload)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "adjust gas cost (warm access)" in {
        val call = fxt.ExecuteCall(op = CALL, toAlreadyAccessed = true, value = 0)
        val expectedGas = fxt.requiredGas + G_warm_storage_read + fxt.expectedMemCost - (G_sset - G_sload)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "external contract terminates abnormally" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.ExecuteCall(op = CALL, context)

      behave.like(callAbnormalTermination(fxt, call))

      "consume all call gas (cold access)" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_cold_account_access + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume all call gas (warm access)" in {
        val call = fxt.ExecuteCall(op = CALL, context, toAlreadyAccessed = true)
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_warm_storage_read + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "calling a non-existent account" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = fxt.ExecuteCall(op = CALL, context)

      behave.like(callNonExistent(fxt, call))

      "consume correct gas (refund call gas, add new account modifier) (cold access)" in {
        val expectedGas = G_cold_account_access + G_callvalue + G_newaccount - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas, add new account modifier) (warm access)" in {
        val call = fxt.ExecuteCall(op = CALL, context, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + G_callvalue + G_newaccount - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(1) // ECDSA recovery
      val invalidSignature = ByteString(Array.fill(128)(0.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.ExecuteCall(
        op = CALL,
        context = context,
        to = contractAddress,
        inputData = invalidSignature,
        inOffset = 0,
        inSize = 128,
        outOffset = 0,
        outSize = 128
      )

      behave.like(callPrecompiled(fxt, call))

      "consume correct gas" in {
        val contractCost = UInt256(3000)
        val expectedGas = contractCost - G_callstipend + G_warm_storage_read + G_callvalue // memory not increased
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {
      behave.like(callSelfdestruct(fxt))
    }

    "calling a program that executes a REVERT" should {
      behave.like(callRevert(fxt))
    }

    "calling a program that executes a SSTORE that clears the storage" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.ExecuteCall(op = CALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe (config.feeSchedule.R_sclear + config.feeSchedule.G_sreset - config.feeSchedule.G_sload)
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

  }

  "CALLCODE" when {

    "external code terminates normally" should {
      val call = fxt.ExecuteCall(op = CALLCODE, outSize = fxt.inputData.size * 2)

      behave.like(callCodeNormalTermination(fxt, call))

      "consume correct gas (refund unused gas) (cold access)" in {
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas - G_callstipend + G_cold_account_access + G_callvalue + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund unused gas) (warm access)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, outSize = fxt.inputData.size * 2, toAlreadyAccessed = true)
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas - G_callstipend + G_warm_storage_read + G_callvalue + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call depth limit is reached" should {

      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val call = fxt.ExecuteCall(op = CALLCODE, context = context)

      behave.like(callDepthLimitReached(fxt, call))

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_cold_account_access + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm access)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, context = context, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call value is greater than balance" should {

      val call = fxt.ExecuteCall(op = CALLCODE, value = fxt.initialBalance + 1)

      behave.like(callValueGreaterThanBalance(fxt, call))

      "consume correct gas (refund call gas) (cold)" in {
        val expectedGas = G_cold_account_access + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, value = fxt.initialBalance + 1, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call value is zero" should {
      val call = fxt.ExecuteCall(op = CALLCODE, value = 0)

      "adjust gas cost (cold access)" in {
        val expectedGas = fxt.requiredGas + G_cold_account_access + fxt.expectedMemCost - (G_sset - G_sload)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "adjust gas cost (warm access)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, toAlreadyAccessed = true, value = 0)
        val expectedGas = fxt.requiredGas + G_warm_storage_read + fxt.expectedMemCost - (G_sset - G_sload)
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "external code terminates abnormally" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.ExecuteCall(op = CALLCODE, context)

      behave.like(callAbnormalTermination(fxt, call))

      "consume all call gas (cold)" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_cold_account_access + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume all call gas (warm)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, context, toAlreadyAccessed = true)
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_warm_storage_read + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "external account does not exist" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = fxt.ExecuteCall(op = CALLCODE, context)

      behave.like(callCodeNonExistent(fxt, call))

      "consume correct gas (refund call gas) (cold)" in {
        val expectedGas = G_cold_account_access + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm)" in {
        val call = fxt.ExecuteCall(op = CALLCODE, context, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(2) // SHA256
      val inputData = ByteString(Array.fill(128)(1.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.ExecuteCall(
        op = CALLCODE,
        context = context,
        to = contractAddress,
        inputData = inputData,
        inOffset = 0,
        inSize = 128,
        outOffset = 128,
        outSize = 32
      )

      behave.like(callCodePrecompiled(fxt, call))

      "consume correct gas" in {
        val contractCost = 60 + 12 * wordsForBytes(inputData.size)
        val expectedGas =
          contractCost - G_callstipend + G_warm_storage_read + G_callvalue + config.calcMemCost(128, 128, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructProgram)
      val call = fxt.ExecuteCall(op = CALLCODE, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_selfdestruct
      }

    }

    "calling a program that executes a SSTORE that clears the storage" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.ExecuteCall(op = CALLCODE, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe (config.feeSchedule.R_sclear + config.feeSchedule.G_sreset - config.feeSchedule.G_sload)
      }
    }
  }

  "DELEGATECALL" when {
    "external code terminates normally" should {
      val call = fxt.ExecuteCall(op = DELEGATECALL, outSize = fxt.inputData.size / 4)

      behave.like(delegateCallNormalTermination(fxt, call))

      "consume correct gas (refund unused gas) (cold)" in {
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas + G_cold_account_access + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund unused gas) (warm)" in {
        val call = fxt.ExecuteCall(op = DELEGATECALL, outSize = fxt.inputData.size / 4, toAlreadyAccessed = true)
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas + G_warm_storage_read + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "call depth limit is reached" should {

      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val call = fxt.ExecuteCall(op = DELEGATECALL, context = context)

      behave.like(callDepthLimitReached(fxt, call))

      "consume correct gas (refund call gas) (cold)" in {
        val expectedGas = G_cold_account_access + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm)" in {
        val call = fxt.ExecuteCall(op = DELEGATECALL, context = context, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "external code terminates abnormally" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.ExecuteCall(op = DELEGATECALL, context)

      behave.like(callAbnormalTermination(fxt, call))

      "consume all call gas (cold)" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_cold_account_access + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }

      "consume all call gas (warm)" in {
        val call = fxt.ExecuteCall(op = DELEGATECALL, context, toAlreadyAccessed = true)
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_warm_storage_read + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "external account does not exist" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = fxt.ExecuteCall(op = DELEGATECALL, context)

      behave.like(callCodeNonExistent(fxt, call))

      "consume correct gas (refund call gas) (cold)" in {
        val expectedGas = G_cold_account_access + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }

      "consume correct gas (refund call gas) (warm)" in {
        val call = fxt.ExecuteCall(op = DELEGATECALL, context, toAlreadyAccessed = true)
        val expectedGas = G_warm_storage_read + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(3) // RIPEMD160
      val inputData = ByteString(Array.fill(128)(1.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.ExecuteCall(
        op = DELEGATECALL,
        context = context,
        to = contractAddress,
        inputData = inputData,
        inOffset = 0,
        inSize = 128,
        outOffset = 128,
        outSize = 32
      )

      behave.like(delegateCallPrecompile(fxt, call))

      "consume correct gas" in {
        val contractCost = 600 + 120 * wordsForBytes(inputData.size)
        val expectedGas = contractCost + G_warm_storage_read + config.calcMemCost(128, 128, 20)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructProgram)
      val call = fxt.ExecuteCall(op = DELEGATECALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_selfdestruct
      }

    }

    "calling a program that executes a SSTORE that clears the storage" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.ExecuteCall(op = DELEGATECALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe (config.feeSchedule.R_sclear + config.feeSchedule.G_sreset - config.feeSchedule.G_sload)
      }
    }

    /** This test should result in an OutOfGas error as (following the equations. on the DELEGATECALL opcode in the YP):
      * DELEGATECALL cost = memoryCost + C_extra + C_gascap
      * and
      * memoryCost = 0 (result written were input was)
      * C_gascap = u_s[0] = UInt256.MaxValue - C_extra + 1
      * Then
      * CALL cost = UInt256.MaxValue + 1
      * As the starting gas (startGas = C_extra - 1) is much lower than the cost this should result in an OutOfGas exception
      */
    "gas cost bigger than available gas DELEGATECALL (cold)" should {

      val c_extra = config.feeSchedule.G_cold_account_access
      val startGas = c_extra - 1
      val gas = UInt256.MaxValue - c_extra + 1 //u_s[0]
      val context: PC = fxt.context.copy(startGas = startGas)
      val call = fxt.ExecuteCall(
        op = DELEGATECALL,
        gas = gas,
        context = context,
        outOffset = UInt256.Zero
      )
      "return an OutOfGas error" in {
        call.stateOut.error shouldBe Some(OutOfGas)
        call.stateOut.accessedAddresses shouldNot contain(fxt.extAddr)
      }
    }

    "gas cost bigger than available gas DELEGATECALL (warm)" should {

      val c_extra = config.feeSchedule.G_warm_storage_read
      val startGas = c_extra - 1
      val gas = UInt256.MaxValue - c_extra + 1 //u_s[0]
      val context: PC = fxt.context.copy(startGas = startGas)
      val call = fxt.ExecuteCall(
        op = DELEGATECALL,
        gas = gas,
        context = context,
        outOffset = UInt256.Zero,
        toAlreadyAccessed = true
      )
      "return an OutOfGas error" in {
        call.stateOut.error shouldBe Some(OutOfGas)
        call.stateOut.accessedAddresses should contain(fxt.extAddr)
      }
    }
  }
}

class Eip2929CallOpFixture(config: EvmConfig, forkBlockHeight: Int)
    extends CallOpFixture(config, MockWorldState(touchedAccounts = Set.empty)) {

  override val fakeHeader: BlockHeader =
    BlockFixtures.ValidBlock.header.copy(number = forkBlockHeight, unixTimestamp = 0)

  override val requiredGas: BigInt = {
    val storageCost = 3 * (config.feeSchedule.G_sset + config.feeSchedule.G_cold_sload)
    val memCost = config.calcMemCost(0, 0, 32)
    val copyCost = config.feeSchedule.G_copy * wordsForBytes(32)

    extCode.linearConstGas(config) + storageCost + memCost + copyCost
  }
}
