package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{Account, Address}
import org.scalatest.{Matchers, WordSpec}
import MockWorldState.{PC, PS}
import akka.util.ByteString

class CreateOpcodeSpec extends WordSpec with Matchers {
  val config = EvmConfig.PostEIP160Config
  import config.feeSchedule._

  object fxt {

    val creatorAddr = Address(0xcafe)
    val endowment: UInt256 = 123
    val initWorld = MockWorldState().saveAccount(creatorAddr, Account.Empty.increaseBalance(endowment))
    val newAddr = initWorld.increaseCreatorNonceAndCreateAddress(creatorAddr)._1

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

    val initPart = Assembly(
      PUSH1, 42,
      PUSH1, 0,
      SSTORE, //store an arbitrary value
      PUSH1, contractCode.code.size,
      DUP1,
      PUSH1, 16,
      PUSH1, 0,
      CODECOPY,
      PUSH1, 0,
      RETURN
    )

    val createCode = Assembly(initPart.byteCode ++ contractCode.byteCode: _*)

    val copyCodeGas = G_copy * wordsForBytes(contractCode.code.size) + config.calcMemCost(0, 0, contractCode.code.size)
    val storeGas = G_sset
    val gasRequiredForInit = initPart.linearConstGas(config) + copyCodeGas + storeGas
    val depositGas = config.calcCodeDepositCost(contractCode.code)
    val gasRequiredForCreation = gasRequiredForInit + depositGas + G_create

    val env = ExecEnv(creatorAddr, Address(0), Address(0), 1, ByteString.empty, 0, Program(ByteString.empty), null, 0)
    val context: PC = ProgramContext(env, 2 * gasRequiredForCreation, initWorld, config)
  }

  case class CreateResult(context: PC = fxt.context, value: UInt256 = fxt.endowment) {
    val mem = Memory.empty.store(0, fxt.createCode.code)
    val stack = Stack.empty().push(Seq[UInt256](fxt.createCode.code.size, 0, value))
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
    }

    "initialization code fails" should {
      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit / 2)
      val result = CreateResult(context = context)

      "not modify world state" in {
        result.world shouldEqual context.world
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        val expectedGas = G_create + config.gasCap(context.startGas - G_create)
        result.stateOut.gasUsed shouldEqual expectedGas
      }
    }

    "initialization code runs normally but there's not enough gas to deposit code" should {
      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit + fxt.depositGas / 2)
      val result = CreateResult(context = context)

      "throw OOG" in {
        result.stateOut.error shouldEqual Some(OutOfGas)
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
}
