package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{Account, Address}
import org.scalatest.{Matchers, WordSpec}
import GasFee._
import MockWorldState.{PC, PS}
import akka.util.ByteString

class CreateOpcodeSpec extends WordSpec with Matchers {
  object fxt {

    val creatorAddr = Address(0xcafebabe)
    val endowment = 123
    val (newAddr, initWorld) =
      MockWorldState().saveAccount(creatorAddr, Account.Empty.increaseBalance(endowment)).newAddress(creatorAddr)

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
      PUSH1, contractCode.code.size,
      DUP1,
      PUSH1, 11,
      PUSH1, 0,
      CODECOPY,
      PUSH1, 0,
      RETURN
    )

    val createCode = Assembly(initPart.byteCode ++ contractCode.byteCode: _*)

    val gasRequiredForInit = initPart.linearConstGas
    val gasRequiredForCreation = gasRequiredForInit + calcCodeDepositCost(contractCode.code)

    val env = ExecEnv(creatorAddr, Address(0), Address(0), 1, ByteString.empty, 0, Program(ByteString.empty), null, 0)
    val context: PC = ProgramContext(env, 2 * gasRequiredForCreation, initWorld)
  }

  case class CreateResult(context: PC = fxt.context, value: UInt256 = fxt.endowment) {
    val mem = Memory.empty.store(0, fxt.createCode.code)
    val stack = Stack.empty().push(Seq[UInt256](fxt.createCode.code.size, 0, value))
    val stateIn: PS = ProgramState(context).withStack(stack).withMemory(mem)
    val stateOut: PS = CREATE.execute(stateIn)

    val world = stateOut.world
    val created = world.getAccount(fxt.newAddr).getOrElse(Account.Empty)
    val newAddr = Address(stack.pop._1)
  }


  "CREATE" when {
    "initialization code executes normally" should {

      val result = CreateResult()

      "create a new contract" in {
        val newAccount = result.world.getAccount(fxt.newAddr).get
        newAccount.balance shouldEqual fxt.endowment
        result.world.getCode(fxt.newAddr) shouldEqual fxt.contractCode.code
      }

      "update sender (creator) account" in {

      }

      "return the new contract's address" in {
        result.newAddr shouldEqual fxt.newAddr
      }

      "consume correct gas" in {

      }
    }

    "initialization code fails" should {
      "not modify world state" in {

      }

      "return 0" in {

      }

      "consume correct gas" in {

      }
    }

    "initialization code runs normally but there's not enough gas to deposit code" should {
      "not modify world state" in {

      }

      "return 0" in {

      }

      "consume correct gas" in {

      }
    }

    "call depth limit is reached" should {
      "not modify world state" in {

      }

      "return 0" in {

      }

      "consume correct gas" in {

      }
    }

    "endowment value is greater than balance" should {
      "not modify world state" in {

      }

      "return 0" in {

      }

      "consume correct gas" in {

      }
    }
  }
}
