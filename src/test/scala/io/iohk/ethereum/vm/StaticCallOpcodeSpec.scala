package io.iohk.ethereum.vm

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState._

// scalastyle:off object.name
class StaticCallOpcodeSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  val config: EvmConfig = EvmConfig.ByzantiumConfigBuilder(blockchainConfig)
  val startState: MockWorldState = MockWorldState(touchedAccounts = Set.empty, noEmptyAccountsCond = true)

  val fxt = new CallOpFixture(config, startState)

  def stateWithProgram(code: Assembly): MockWorldState =
    fxt.worldWithoutExtAccount.saveAccount(fxt.extAddr, fxt.accountWithCode(code.code)).saveCode(fxt.extAddr, code.code)

  "STATICCALL" should {

    "calling a program that executes a state-changing opcodes" should {
      val programsWithStateChangingOpcodes = Map(
        CREATE -> stateWithProgram(
          Assembly(
            PUSH1,
            0,
            PUSH1,
            0,
            PUSH1,
            0,
            CREATE
          )
        ),
        SELFDESTRUCT -> stateWithProgram(Assembly(PUSH20, fxt.extAddr.bytes, SELFDESTRUCT)),
        SSTORE -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 10, SSTORE))
      )

      programsWithStateChangingOpcodes.foreach { case (op, worldState) =>
        val context: PC = fxt.context.copy(world = worldState)
        val staticcall = fxt.CallResult(op = STATICCALL, context)
        val call = fxt.CallResult(op = CALL, context)

        s"Opcode $op" should {
          "not modify world state" in {
            staticcall.world shouldEqual worldState
          }

          "balance should be equal to initial balance" in {
            staticcall.ownBalance shouldEqual fxt.initialBalance
            staticcall.ownBalance should be > call.ownBalance
          }
        }
      }
    }

    "calling a program that executes a logging opcodes" should {
      val programsWithLoggingOpcodes = Map(
        LOG0 -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 0, LOG0)),
        LOG1 -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 0, PUSH1, 0, LOG1)),
        LOG2 -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, LOG2)),
        LOG3 -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, LOG3)),
        LOG4 -> stateWithProgram(Assembly(PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, PUSH1, 0, LOG4))
      )

      programsWithLoggingOpcodes.foreach { case (op, worldState) =>
        val context: PC = fxt.context.copy(world = worldState)
        val staticcall = fxt.CallResult(op = STATICCALL, context)
        val call = fxt.CallResult(op = CALL, context)

        s"Opcode $op" should {
          "should not append any logs" in {
            call.stateOut.logs.size should be > 0
            staticcall.stateOut.logs.size shouldEqual 0
          }
        }
      }
    }

  }
}
