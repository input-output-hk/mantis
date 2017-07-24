package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{Account, Address}
import org.scalatest.{Matchers, WordSpec}
import MockWorldState.{PC, PS}
import akka.util.ByteString

class CreateOpcodeSpec extends WordSpec with Matchers {
  val config = EvmConfig.PostEIP160ConfigBuilder(None)
  import config.feeSchedule._

  object fxt {

    val creatorAddr = Address(0xcafe)
    val endowment: UInt256 = 123
    val initWorld = MockWorldState().saveAccount(creatorAddr, Account.empty().increaseBalance(endowment))
    val newAddr = initWorld.createAddressWithOpCode(creatorAddr)._1

    // doubles the value passed in the input data
    val contractCode = Assembly(
      PUSH1, 0,
      CALLDATALOAD,
      DUP1,
      ADD,
      PUSH1, 0,
      MSTORE,
      PUSH1, 32,
      PUSH1, 0,
      RETURN
    )

    def initPart(contractCodeSize: Int): Assembly = Assembly(
      PUSH1, 42,
      PUSH1, 0,
      SSTORE, //store an arbitrary value
      PUSH1, contractCodeSize,
      DUP1,
      PUSH1, 16,
      PUSH1, 0,
      CODECOPY,
      PUSH1, 0,
      RETURN
    )

    val initWithSelfDestruct = Assembly(
      PUSH1, creatorAddr.toUInt256.toInt,
      SELFDESTRUCT
    )

    val initWithSstoreWithClear = Assembly(
      //Save a value to the storage
      PUSH1, 10,
      PUSH1, 0,
      SSTORE,

      //Clear the store
      PUSH1, 0,
      PUSH1, 0,
      SSTORE
    )

    val createCode = Assembly(initPart(contractCode.code.size).byteCode ++ contractCode.byteCode: _*)

    val copyCodeGas = G_copy * wordsForBytes(contractCode.code.size) + config.calcMemCost(0, 0, contractCode.code.size)
    val storeGas = G_sset
    val gasRequiredForInit = initPart(contractCode.code.size).linearConstGas(config) + copyCodeGas + storeGas
    val depositGas = config.calcCodeDepositCost(contractCode.code)
    val gasRequiredForCreation = gasRequiredForInit + depositGas + G_create

    val env = ExecEnv(creatorAddr, Address(0), Address(0), 1, ByteString.empty, 0, Program(ByteString.empty), null, 0)
    val context: PC = ProgramContext(env, Address(0), 2 * gasRequiredForCreation, initWorld, config)
  }

  case class CreateResult(context: PC = fxt.context, value: UInt256 = fxt.endowment, createCode: ByteString = fxt.createCode.code) {
    val mem = Memory.empty.store(0, createCode)
    val stack = Stack.empty().push(Seq[UInt256](createCode.size, 0, value))
    val stateIn: PS = ProgramState(context).withStack(stack).withMemory(mem)
    val stateOut: PS = CREATE.execute(stateIn)

    val world = stateOut.world
    val returnValue = stateOut.stack.pop._1
  }


  "CREATE" when {
    "initialization code executes normally" should {

      val result = CreateResult()

      "create a new contract" in {
        val newAccount = result.world.getGuaranteedAccount(fxt.newAddr)

        newAccount.balance shouldEqual fxt.endowment
        result.world.getCode(fxt.newAddr) shouldEqual fxt.contractCode.code
        result.world.getStorage(fxt.newAddr).load(0) shouldEqual 42
      }

      "update sender (creator) account" in {
        val initialCreator = result.context.world.getGuaranteedAccount(fxt.creatorAddr)
        val updatedCreator = result.world.getGuaranteedAccount(fxt.creatorAddr)

        updatedCreator.balance shouldEqual initialCreator.balance - fxt.endowment
        updatedCreator.nonce shouldEqual initialCreator.nonce + 1
      }

      "return the new contract's address" in {
        Address(result.returnValue) shouldEqual fxt.newAddr
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual fxt.gasRequiredForCreation
      }

      "step forward" in {
        result.stateOut.pc shouldEqual result.stateIn.pc + 1
      }
    }

    "initialization code fails" should {
      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit / 2)
      val result = CreateResult(context = context)

      "not modify world state except for the creator's nonce" in {
        val creatorsAccount = context.world.getGuaranteedAccount(fxt.creatorAddr)
        val expectedWorld = context.world.saveAccount(fxt.creatorAddr, creatorsAccount.copy(nonce = creatorsAccount.nonce + 1))
        result.world shouldEqual expectedWorld
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        val expectedGas = G_create + config.gasCap(context.startGas - G_create)
        result.stateOut.gasUsed shouldEqual expectedGas
      }

      "step forward" in {
        result.stateOut.pc shouldEqual result.stateIn.pc + 1
      }
    }

    "initialization code runs normally but there's not enough gas to deposit code" should {
      val depositGas = fxt.depositGas * 101 / 100
      val availableGasDepth0 = fxt.gasRequiredForInit + depositGas
      val availableGasDepth1 = config.gasCap(availableGasDepth0)
      val gasUsedInInit = fxt.gasRequiredForInit + fxt.depositGas

      require(
        gasUsedInInit < availableGasDepth0 && gasUsedInInit > availableGasDepth1,
        "Regression: capped startGas in the VM at depth 1, should be used a base for code deposit gas check"
      )

      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit + depositGas)
      val result = CreateResult(context = context)

      "consume all gas passed to the init code" in {
        val expectedGas = G_create + config.gasCap(context.startGas - G_create)
        result.stateOut.gasUsed shouldEqual expectedGas
      }

      "not modify world state except for the creator's nonce" in {
        val creatorsAccount = context.world.getGuaranteedAccount(fxt.creatorAddr)
        val expectedWorld = context.world.saveAccount(fxt.creatorAddr, creatorsAccount.copy(nonce = creatorsAccount.nonce + 1))
        result.world shouldEqual expectedWorld
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }
    }

    "call depth limit is reached" should {
      val env = fxt.env.copy(callDepth = EvmConfig.MaxCallDepth)
      val context: PC = fxt.context.copy(env = env)
      val result = CreateResult(context = context)

      "not modify world state" in {
        result.world shouldEqual context.world
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual G_create
      }
    }

    "endowment value is greater than balance" should {
      val result = CreateResult(value = fxt.endowment * 2)

      "not modify world state" in {
        result.world shouldEqual result.context.world
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual G_create
      }
    }
  }

  "initialization includes SELFDESTRUCT opcode" should {
    val gasRequiredForInit = fxt.initWithSelfDestruct.linearConstGas(config) + G_newaccount
    val gasRequiredForCreation = gasRequiredForInit + G_create

    val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation)
    val result = CreateResult(context = context, createCode = fxt.initWithSelfDestruct.code)

    "refund the correct amount of gas" in {
      result.stateOut.gasRefund shouldBe result.stateOut.config.feeSchedule.R_selfdestruct
    }

  }

  "initialization includes a SSTORE opcode that clears the storage" should {

    val codeExecGas = G_sreset + G_sset
    val gasRequiredForInit = fxt.initWithSstoreWithClear.linearConstGas(config) + codeExecGas
    val gasRequiredForCreation = gasRequiredForInit + G_create

    val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation)
    val call = CreateResult(context = context, createCode = fxt.initWithSstoreWithClear.code)

    "refund the correct amount of gas" in {
      call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_sclear
    }

  }

  "maxCodeSize check is enabled" should {
    val maxCodeSize = 30
    val ethConfig = EvmConfig.PostEIP160ConfigBuilder(Some(maxCodeSize))

    val context: PC = fxt.context.copy(startGas = Int.MaxValue, config = ethConfig)

    val gasConsumedIfError = G_create + config.gasCap(context.startGas - G_create) //Gas consumed by CREATE opcode if an error happens

    "result in an out of gas if the code is larger than the limit" in {
      val codeSize = maxCodeSize + 1
      val largeContractCode = Assembly((0 until codeSize).map(_ => Assembly.OpCodeAsByteCode(STOP)): _*)
      val createCode = Assembly(fxt.initPart(largeContractCode.code.size).byteCode ++ largeContractCode.byteCode: _*).code
      val call = CreateResult(context = context, createCode = createCode)

      call.stateOut.error shouldBe None
      call.stateOut.gasUsed shouldBe gasConsumedIfError
    }

    "not result in an out of gas if the code is smaller than the limit" in {
      val codeSize = maxCodeSize - 1
      val largeContractCode = Assembly((0 until codeSize).map(_ => Assembly.OpCodeAsByteCode(STOP)): _*)
      val createCode = Assembly(fxt.initPart(largeContractCode.code.size).byteCode ++ largeContractCode.byteCode: _*).code
      val call = CreateResult(context = context, createCode = createCode)

      call.stateOut.error shouldBe None
      call.stateOut.gasUsed shouldNot be(gasConsumedIfError)
    }

  }
}
