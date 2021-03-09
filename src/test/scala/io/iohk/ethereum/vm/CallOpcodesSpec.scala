package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Account, Address, UInt256}
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.vm.MockWorldState._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import Fixtures.blockchainConfig

// scalastyle:off object.name
// scalastyle:off file.size.limit
class CallOpcodesSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  val config: EvmConfig = EvmConfig.ByzantiumConfigBuilder(blockchainConfig)
  val startState: MockWorldState = MockWorldState(touchedAccounts = Set.empty)
  import config.feeSchedule._

  val fxt = new CallOpFixture(config, startState)

  "CALL" when {

    "external contract terminates normally" should {

      val call = fxt.CallResult(op = CALL)

      "update external account's storage" in {
        call.ownStorage shouldEqual MockStorage.Empty
        call.extStorage.data.size shouldEqual 3
      }

      "update external account's balance" in {
        call.extBalance shouldEqual call.value
        call.ownBalance shouldEqual fxt.initialBalance - call.value
      }

      "pass correct addresses and value" in {
        Address(call.extStorage.load(fxt.ownerOffset)) shouldEqual fxt.extAddr
        Address(call.extStorage.load(fxt.callerOffset)) shouldEqual fxt.ownerAddr
        call.extStorage.load(fxt.valueOffset) shouldEqual call.value.toBigInt
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "should store contract's return data in memory" in {
        //here the passed data size is equal to the contract's return data size (half of the input data)

        val expectedData = fxt.inputData.take(fxt.inputData.size / 2)
        val actualData = call.stateOut.memory.load(call.outOffset, call.outSize)._1
        actualData shouldEqual expectedData

        val expectedSize = (call.outOffset + call.outSize).toInt
        val actualSize = call.stateOut.memory.size
        expectedSize shouldEqual actualSize
      }

      "consume correct gas (refund unused gas)" in {
        val expectedGas = fxt.requiredGas - G_callstipend + G_call + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val call = fxt.CallResult(op = CALL, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is greater than balance" should {

      val call = fxt.CallResult(op = CALL, value = fxt.initialBalance + 1)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + config.calcMemCost(32, 32, 16)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is zero" should {
      val call = fxt.CallResult(op = CALL, value = 0)

      "adjust gas cost" in {
        val expectedGas = fxt.requiredGas + G_call + fxt.expectedMemCost - (G_sset - G_sreset)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external contract terminates abnormally" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.CallResult(op = CALL, context)

      "should not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }

      "extend memory" in {
        UInt256(call.stateOut.memory.size) shouldEqual call.outOffset + call.outSize
      }
    }

    "calling a non-existent account" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)

      val call = fxt.CallResult(op = CALL, context)

      "create new account and add to its balance" in {
        call.extBalance shouldEqual call.value
        call.ownBalance shouldEqual fxt.initialBalance - call.value
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "consume correct gas (refund call gas, add new account modifier)" in {
        val expectedGas = G_call + G_callvalue + G_newaccount - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(1) // ECDSA recovery
      val invalidSignature = ByteString(Array.fill(128)(0.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.CallResult(
        op = CALL,
        context = context,
        to = contractAddress,
        inputData = invalidSignature,
        inOffset = 0,
        inSize = 128,
        outOffset = 0,
        outSize = 128
      )

      "compute a correct result" in {
        // For invalid signature the return data should be empty, so the memory should not be modified.
        // This is more interesting than checking valid signatures which are tested elsewhere
        val (result, _) = call.stateOut.memory.load(call.outOffset, call.outSize)
        val expected = invalidSignature

        result shouldEqual expected
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "update precompiled contract's balance" in {
        call.extBalance shouldEqual call.value + 1
        call.ownBalance shouldEqual fxt.initialBalance - call.value
      }

      "consume correct gas" in {
        val contractCost = UInt256(3000)
        val expectedGas = contractCost - G_callstipend + G_call + G_callvalue // memory not increased
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {

      "refund the correct amount of gas" in {
        val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructProgram)
        val call = fxt.CallResult(op = CALL, context)
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_selfdestruct
      }

      "not refund gas if account was already self destructed" in {
        val context: PC =
          fxt.context.copy(world = fxt.worldWithSelfDestructProgram, initialAddressesToDelete = Set(fxt.extAddr))
        val call = fxt.CallResult(op = CALL, context)
        call.stateOut.gasRefund shouldBe 0
      }

      "destruct ether if own address equals refund address" in {
        val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructSelfProgram)
        val call = fxt.CallResult(op = CALL, context)
        call.stateOut.world.getGuaranteedAccount(fxt.extAddr).balance shouldEqual UInt256.Zero
        call.stateOut.addressesToDelete.contains(fxt.extAddr) shouldBe true
      }
    }

    "calling a program that executes a REVERT" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithRevertProgram)
      val call = fxt.CallResult(op = CALL, context)

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "store cause of reversion in memory" in {
        val resultingMemoryBytes = call.stateOut.memory.load(call.outOffset, 1)._1
        resultingMemoryBytes shouldEqual ByteString(fxt.valueToReturn.toByte)
      }

      "extend memory" in {
        UInt256(call.stateOut.memory.size) shouldEqual call.outOffset + call.outSize
      }
    }

    "calling a program that executes a SSTORE that clears the storage" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.CallResult(op = CALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_sclear
      }

    }

    "more gas than available is provided" should {
      def call(config: EvmConfig): fxt.CallResult = {
        val context: PC = fxt.context.copy(evmConfig = config)
        fxt.CallResult(op = CALL, context = context, gas = UInt256.MaxValue / 2)
      }

      def callVarMemCost(config: EvmConfig): fxt.CallResult = {

        /** Amount of memory which causes the improper OOG exception, if we don take memcost into account
          * during calculation of post EIP150 CALLOp gasCap: gasCap(state, gas, gExtra + memCost)
          */
        val gasFailingBeforeEIP150Fix = 141072

        val context: PC = fxt.context.copy(evmConfig = config)
        fxt.CallResult(
          op = CALL,
          context = context,
          inOffset = UInt256.Zero,
          inSize = fxt.inputData.size,
          outOffset = fxt.inputData.size,
          outSize = gasFailingBeforeEIP150Fix
        )
      }

      "go OOG before EIP-150" in {
        call(EvmConfig.HomesteadConfigBuilder(blockchainConfig)).stateOut.error shouldEqual Some(OutOfGas)
      }

      "cap the provided gas after EIP-150" in {
        call(EvmConfig.PostEIP150ConfigBuilder(blockchainConfig)).stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "go OOG before EIP-150 becaouse of extensive memory cost" in {
        callVarMemCost(EvmConfig.HomesteadConfigBuilder(blockchainConfig)).stateOut.error shouldEqual Some(OutOfGas)
      }

      "cap memory cost post EIP-150" in {
        val CallResult = callVarMemCost(EvmConfig.PostEIP150ConfigBuilder(blockchainConfig))
        CallResult.stateOut.stack.pop._1 shouldEqual UInt256.One
      }
    }
  }

  "CALLCODE" when {
    "external code terminates normally" should {
      val call = fxt.CallResult(op = CALLCODE, outSize = fxt.inputData.size * 2)

      "update own account's storage" in {
        call.extStorage shouldEqual MockStorage.Empty
        call.ownStorage.data.size shouldEqual 3
      }

      "not update any account's balance" in {
        call.extBalance shouldEqual UInt256.Zero
        call.ownBalance shouldEqual fxt.initialBalance
      }

      "pass correct addresses and value" in {
        Address(call.ownStorage.load(fxt.ownerOffset)) shouldEqual fxt.ownerAddr
        Address(call.ownStorage.load(fxt.callerOffset)) shouldEqual fxt.ownerAddr
        call.ownStorage.load(fxt.valueOffset) shouldEqual call.value.toBigInt
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256(1)
      }

      "should store contract's return data in memory" in {
        //here the passed data size is greater than the contract's return data size

        val expectedData = fxt.inputData.take(fxt.inputData.size / 2).padTo(call.outSize.toInt, 0)
        val actualData = call.stateOut.memory.load(call.outOffset, call.outSize)._1
        actualData shouldEqual expectedData

        val expectedSize = (call.outOffset + call.outSize).toInt
        val actualSize = call.stateOut.memory.size
        expectedSize shouldEqual actualSize
      }

      "consume correct gas (refund unused gas)" in {
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas - G_callstipend + G_call + G_callvalue + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val call = fxt.CallResult(op = CALLCODE, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is greater than balance" should {

      val call = fxt.CallResult(op = CALLCODE, value = fxt.initialBalance + 1)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is zero" should {
      val call = fxt.CallResult(op = CALLCODE, value = 0)

      "adjust gas cost" in {
        val expectedGas = fxt.requiredGas + G_call + fxt.expectedMemCost - (G_sset - G_sreset)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external code terminates abnormally" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.CallResult(op = CALLCODE, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + G_callvalue + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }

      "extend memory" in {
        UInt256(call.stateOut.memory.size) shouldEqual call.outOffset + call.outSize
      }
    }

    "external account does not exist" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = fxt.CallResult(op = CALLCODE, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithoutExtAccount
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256(1)
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(2) // SHA256
      val inputData = ByteString(Array.fill(128)(1.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.CallResult(
        op = CALLCODE,
        context = context,
        to = contractAddress,
        inputData = inputData,
        inOffset = 0,
        inSize = 128,
        outOffset = 128,
        outSize = 32
      )

      "compute a correct result" in {
        val (result, _) = call.stateOut.memory.load(call.outOffset, call.outSize)
        val expected = sha256(inputData)

        result shouldEqual expected
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "not update precompiled contract's balance" in {
        call.extBalance shouldEqual 1
        call.ownBalance shouldEqual fxt.initialBalance
      }

      "consume correct gas" in {
        val contractCost = 60 + 12 * wordsForBytes(inputData.size)
        val expectedGas = contractCost - G_callstipend + G_call + G_callvalue + config.calcMemCost(128, 128, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructProgram)
      val call = fxt.CallResult(op = CALLCODE, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_selfdestruct
      }

    }

    "calling a program that executes a SSTORE that clears the storage" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.CallResult(op = CALLCODE, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_sclear
      }
    }

    "more gas than available is provided" should {
      def call(config: EvmConfig): fxt.CallResult = {
        val context: PC = fxt.context.copy(evmConfig = config)
        fxt.CallResult(op = CALLCODE, context = context, gas = UInt256.MaxValue / 2)
      }

      "go OOG before EIP-150" in {
        call(EvmConfig.HomesteadConfigBuilder(blockchainConfig)).stateOut.error shouldEqual Some(OutOfGas)
      }

      "cap the provided gas after EIP-150" in {
        call(EvmConfig.PostEIP150ConfigBuilder(blockchainConfig)).stateOut.stack.pop._1 shouldEqual UInt256.One
      }
    }
  }

  "DELEGATECALL" when {
    "external code terminates normally" should {
      val call = fxt.CallResult(op = DELEGATECALL, outSize = fxt.inputData.size / 4)

      "update own account's storage" in {
        call.extStorage shouldEqual MockStorage.Empty
        call.ownStorage.data.size shouldEqual 3
      }

      "not update any account's balance" in {
        call.extBalance shouldEqual UInt256.Zero
        call.ownBalance shouldEqual fxt.initialBalance
      }

      "pass correct addresses and value" in {
        Address(call.ownStorage.load(fxt.ownerOffset)) shouldEqual fxt.ownerAddr
        Address(call.ownStorage.load(fxt.callerOffset)) shouldEqual fxt.callerAddr
        call.ownStorage.load(fxt.valueOffset) shouldEqual fxt.context.value.toBigInt
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256(1)
      }

      "should store contract's return data in memory" in {
        //here the passed data size is less than the contract's return data size

        val expectedData = fxt.inputData.take(call.outSize.toInt)
        val actualData = call.stateOut.memory.load(call.outOffset, call.outSize)._1
        actualData shouldEqual expectedData

        val expectedSize = (call.outOffset + call.outSize).toInt
        val actualSize = call.stateOut.memory.size
        expectedSize shouldEqual actualSize
      }

      "consume correct gas (refund unused gas)" in {
        val expectedMemCost = config.calcMemCost(fxt.inputData.size, fxt.inputData.size, call.outSize)
        val expectedGas = fxt.requiredGas + G_call + expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val call = fxt.CallResult(op = DELEGATECALL, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external code terminates abnormally" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = fxt.CallResult(op = DELEGATECALL, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }

      "extend memory" in {
        UInt256(call.stateOut.memory.size) shouldEqual call.outOffset + call.outSize
      }
    }

    "external account does not exist" should {
      val context: PC = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = fxt.CallResult(op = DELEGATECALL, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithoutExtAccount
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256(1)
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + fxt.expectedMemCost
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a precompiled contract" should {
      val contractAddress = Address(3) // RIPEMD160
      val inputData = ByteString(Array.fill(128)(1.toByte))
      val world = fxt.worldWithoutExtAccount.saveAccount(contractAddress, Account(balance = 1))
      val context: PC = fxt.context.copy(world = world)
      val call = fxt.CallResult(
        op = DELEGATECALL,
        context = context,
        to = contractAddress,
        inputData = inputData,
        inOffset = 0,
        inSize = 128,
        outOffset = 128,
        outSize = 32
      )

      "compute a correct result" in {
        val (result, _) = call.stateOut.memory.load(call.outOffset, call.outSize)
        val expected = ByteUtils.padLeft(ripemd160(inputData), 32)

        result shouldEqual expected
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual UInt256.One
      }

      "not update precompiled contract's balance" in {
        call.extBalance shouldEqual 1
        call.ownBalance shouldEqual fxt.initialBalance
      }

      "consume correct gas" in {
        val contractCost = 600 + 120 * wordsForBytes(inputData.size)
        val expectedGas = contractCost + G_call + config.calcMemCost(128, 128, 20)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a program that executes a SELFDESTRUCT" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSelfDestructProgram)
      val call = fxt.CallResult(op = DELEGATECALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_selfdestruct
      }

    }

    "calling a program that executes a SSTORE that clears the storage" should {

      val context: PC = fxt.context.copy(world = fxt.worldWithSstoreWithClearProgram)
      val call = fxt.CallResult(op = DELEGATECALL, context)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_sclear
      }
    }

    "more gas than available is provided" should {
      def call(config: EvmConfig): fxt.CallResult = {
        val context: PC = fxt.context.copy(evmConfig = config)
        fxt.CallResult(op = DELEGATECALL, context = context, gas = UInt256.MaxValue / 2)
      }

      "go OOG before EIP-150" in {
        call(EvmConfig.HomesteadConfigBuilder(blockchainConfig)).stateOut.error shouldEqual Some(OutOfGas)
      }

      "cap the provided gas after EIP-150" in {
        call(EvmConfig.PostEIP150ConfigBuilder(blockchainConfig)).stateOut.stack.pop._1 shouldEqual UInt256.One
      }
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
  "gas cost bigger than available gas DELEGATECALL" should {

    val c_extra = config.feeSchedule.G_call
    val startGas = c_extra - 1
    val gas = UInt256.MaxValue - c_extra + 1 //u_s[0]
    val context: PC = fxt.context.copy(startGas = startGas)
    val call = fxt.CallResult(
      op = DELEGATECALL,
      gas = gas,
      context = context,
      outOffset = UInt256.Zero
    )
    "return an OutOfGas error" in {
      call.stateOut.error shouldBe Some(OutOfGas)
    }
  }

  "CallOpCodes" when {

    Seq(CALL, CALLCODE, DELEGATECALL).foreach { opCode =>
      s"$opCode processes returned data" should {

        "handle memory expansion properly" in {

          val inputData = ByteString(Array[Byte](1).padTo(32, 1.toByte))
          val context: PC = fxt.context.copy(world = fxt.worldWithReturnSingleByteCode)

          val table = Table[Int](
            "Out Offset",
            0,
            inputData.size / 2,
            inputData.size * 2
          )

          forAll(table) { outOffset =>
            val call = fxt.CallResult(
              op = opCode,
              outSize = inputData.size,
              outOffset = outOffset,
              context = context,
              inputData = inputData
            )

            val expectedSize = inputData.size + outOffset
            val expectedMemoryBytes =
              call.stateIn.memory.store(outOffset, fxt.valueToReturn.toByte).load(0, expectedSize)._1
            val resultingMemoryBytes = call.stateOut.memory.load(0, expectedSize)._1

            call.stateOut.memory.size shouldEqual expectedSize
            resultingMemoryBytes shouldEqual expectedMemoryBytes

          }
        }
      }
    }
  }
}
