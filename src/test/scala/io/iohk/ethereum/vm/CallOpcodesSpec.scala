package io.iohk.ethereum.vm

import akka.util.ByteString
import org.scalatest.{Matchers, WordSpec}
import Assembly._
import GasFee._
import io.iohk.ethereum.domain.{Account, Address}

class CallOpcodesSpec extends WordSpec with Matchers {

  object fxt {

    val ownerAddr = Address(0xcafebabe)
    val extAddr = Address(0xfacefeed)
    val callerAddr = Address(0xdeadbeef)

    val ownerOffset = DataWord(0)
    val callerOffset = DataWord(1)
    val valueOffset = DataWord(2)

    val extCode = Assembly(
      //store owner address
      ADDRESS,
      PUSH1, ownerOffset.intValue,
      SSTORE,

      //store caller address
      CALLER,
      PUSH1, callerOffset.intValue,
      SSTORE,

      //store call value
      CALLVALUE,
      PUSH1, valueOffset.intValue,
      SSTORE,

      // return unmodified input data
      CALLDATASIZE,
      PUSH1, 0,
      DUP2,
      DUP2,
      DUP1,
      CALLDATACOPY,
      RETURN
    )

    val inputData = Generators.getDataWordGen().sample.get.bytes
    val initialBalance = 1000

    val requiredGas = {
      val storageCost = 3 * G_sset
      val memCost = calcMemCost(0, 0, 32)
      val copyCost = G_copy * wordsForBytes(32)
      extCode.linearConstGas + storageCost + memCost + copyCost
    }

    val gasMargin = 13

    val initialOwnerAccount = Account(0, initialBalance, Storage.Empty.storageRoot, ByteString.empty)

    val extProgram = extCode.program
    val initialExtAccount = Account(0, 0, Storage.Empty.storageRoot, extProgram.codeHash)

    val invalidProgram = Program(extProgram.code.dropRight(1) :+ INVALID.code)
    val accountWithInvalidProgram = Account(0, 0, Storage.Empty.storageRoot, invalidProgram.codeHash)

    val worldWithoutExtAccount = MockWorldState().saveAccount(ownerAddr, initialOwnerAccount)
    val worldWithExtAccount = worldWithoutExtAccount.saveAccount(extAddr, initialExtAccount)
      .saveCode(extProgram.codeHash, extProgram.code)
    val worldWithInvalidProgram = worldWithoutExtAccount.saveAccount(extAddr, accountWithInvalidProgram)
      .saveCode(invalidProgram.codeHash, invalidProgram.code)

    val env = ExecEnv(ownerAddr, callerAddr, callerAddr, 1, ByteString.empty, 123, Program(ByteString.empty), null, 0)
    val context = ProgramContext(env, 1000000, worldWithExtAccount)
  }

  case class CallResult(
    op: CallOp,
    context: ProgramContext = fxt.context,
    inputData: ByteString = fxt.inputData,
    gas: BigInt = fxt.requiredGas + fxt.gasMargin,
    to: Address = fxt.extAddr,
    value: BigInt = fxt.initialBalance / 2
  ) {
    private val params = Seq(
      DataWord(gas),
      DataWord(to.bytes),
      DataWord(value),
      DataWord.Zero,
      DataWord(inputData.size),
      DataWord(inputData.size),
      DataWord(inputData.size)
    ).reverse

    private val paramsForDelegate =
      params.take(4) ++ params.drop(5)

    private val stack = Stack.empty().push(if (op == DELEGATECALL) params.take(4) ++ params.drop(5) else params)
    private val mem = Memory.empty.store(DataWord.Zero, inputData)

    val stateIn = ProgramState(context).withStack(stack).withMemory(mem)
    val stateOut = op.execute(stateIn)
    val world = stateOut.world

    val ownAccount = world.getGuaranteedAccount(context.env.ownerAddr)
    val extAccount = world.getAccount(to).getOrElse(Account.Empty)

    val ownStorage = world.getStorage(ownAccount.storageRoot)
    val extStorage = world.getStorage(extAccount.storageRoot)
  }

  "CALL" when {
    "external contract terminates normally" should {

      val call = CallResult(op = CALL)

      "update external account's storage" in {
        call.ownStorage shouldEqual Storage.Empty
        call.extStorage.toMap.size shouldEqual 3
      }

      "update external account's balance" in {
        call.extAccount.balance shouldEqual call.value
        call.ownAccount.balance shouldEqual fxt.initialBalance - call.value
      }

      "pass correct addresses and value" in {
        Address(call.extStorage.load(fxt.ownerOffset)) shouldEqual fxt.extAddr
        Address(call.extStorage.load(fxt.callerOffset)) shouldEqual fxt.ownerAddr
        call.extStorage.load(fxt.valueOffset) shouldEqual call.value
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund unused gas)" in {
        val expectedGas = fxt.requiredGas - G_callstipend + G_call + G_callvalue + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context = fxt.context.copy(env = fxt.env.copy(callDepth = OpCode.MaxCallDepth))
      val call = CallResult(op = CALL, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is greater than balance" should {

      val call = CallResult(op = CALL, value = fxt.initialBalance + 1)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is zero" should {
      val call = CallResult(op = CALL, value = 0)

      "adjust gas cost" in {
        val expectedGas = fxt.requiredGas + G_call + calcMemCost(32, 32, 32) - (G_sset - G_sreset)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external contract terminates abnormally" should {

      val context = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = CallResult(op = CALL, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + G_callvalue + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "calling a non-existent account" should {

      val context = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = CallResult(op = CALL, context)

      "create new account and add to its balance" in {
        call.extAccount.balance shouldEqual call.value
        call.ownAccount.balance shouldEqual fxt.initialBalance - call.value
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund call gas, add new account modifier)" in {
        val expectedGas = G_call + G_callvalue + G_newaccount - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }
  }

  "CALLCODE" when {
    "external code terminates normally" should {
      val call = CallResult(op = CALLCODE)

      "update own account's storage" in {
        call.extStorage shouldEqual Storage.Empty
        call.ownStorage.toMap.size shouldEqual 3
      }

      "not update any account's balance" in {
        call.extAccount.balance shouldEqual 0
        call.ownAccount.balance shouldEqual fxt.initialBalance
      }

      "pass correct addresses and value" in {
        Address(call.ownStorage.load(fxt.ownerOffset)) shouldEqual fxt.ownerAddr
        Address(call.ownStorage.load(fxt.callerOffset)) shouldEqual fxt.ownerAddr
        call.ownStorage.load(fxt.valueOffset) shouldEqual call.value
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund unused gas)" in {
        val expectedGas = fxt.requiredGas - G_callstipend + G_call + G_callvalue + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context = fxt.context.copy(env = fxt.env.copy(callDepth = OpCode.MaxCallDepth))
      val call = CallResult(op = CALLCODE, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is greater than balance" should {

      val call = CallResult(op = CALLCODE, value = fxt.initialBalance + 1)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call value is zero" should {
      val call = CallResult(op = CALL, value = 0)

      "adjust gas cost" in {
        val expectedGas = fxt.requiredGas + G_call + calcMemCost(32, 32, 32) - (G_sset - G_sreset)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external code terminates abnormally" should {
      val context = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = CallResult(op = CALLCODE, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + G_callvalue + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external account does not exist" should {
      val context = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = CallResult(op = CALLCODE, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithoutExtAccount
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + G_callvalue - G_callstipend + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }
  }

  "DELEGATECALL" when {
    "external code terminates normally" should {
      val call = CallResult(op = DELEGATECALL)

      "update own account's storage" in {
        call.extStorage shouldEqual Storage.Empty
        call.ownStorage.toMap.size shouldEqual 3
      }

      "not update any account's balance" in {
        call.extAccount.balance shouldEqual 0
        call.ownAccount.balance shouldEqual fxt.initialBalance
      }

      "pass correct addresses and value" in {
        Address(call.ownStorage.load(fxt.ownerOffset)) shouldEqual fxt.ownerAddr
        Address(call.ownStorage.load(fxt.callerOffset)) shouldEqual fxt.env.callerAddr
        call.ownStorage.load(fxt.valueOffset) shouldEqual fxt.env.value
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund unused gas)" in {
        val expectedGas = fxt.requiredGas + G_call + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "call depth limit is reached" should {

      val context = fxt.context.copy(env = fxt.env.copy(callDepth = OpCode.MaxCallDepth))
      val call = CallResult(op = DELEGATECALL, context = context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithExtAccount
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external code terminates abnormally" should {
      val context = fxt.context.copy(world = fxt.worldWithInvalidProgram)
      val call = CallResult(op = DELEGATECALL, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithInvalidProgram
      }

      "return 0" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord.Zero
      }

      "consume all call gas" in {
        val expectedGas = fxt.requiredGas + fxt.gasMargin + G_call + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "external account does not exist" should {
      val context = fxt.context.copy(world = fxt.worldWithoutExtAccount)
      val call = CallResult(op = DELEGATECALL, context)

      "not modify world state" in {
        call.world shouldEqual fxt.worldWithoutExtAccount
      }

      "return 1" in {
        call.stateOut.stack.pop._1 shouldEqual DataWord(1)
      }

      "consume correct gas (refund call gas)" in {
        val expectedGas = G_call + calcMemCost(32, 32, 32)
        call.stateOut.gasUsed shouldEqual expectedGas
      }
    }
  }

}
