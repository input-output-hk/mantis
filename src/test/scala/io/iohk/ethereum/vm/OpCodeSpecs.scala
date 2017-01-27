package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.Generators._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class OpCodeSpecs extends FunSuite with Matchers with PropertyChecks {

  def test(op: OpCode)(f: OpCode => Any): Any =
    test(op.toString)(f(op))

  def ignore(op: OpCode)(f: OpCode => Any): Any =
    ignore(op.toString)(f(op))

  def testBinaryOp(binaryOp: BinaryOp): Any = test(binaryOp.toString) {
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = binaryOp.execute(stateIn)

      withStackVerification(binaryOp, stateIn, stateOut) {
        val Right((Seq(a, b), _)) = stateIn.stack.pop(2)
        val Right((result, _)) = stateOut.stack.pop
        result shouldEqual binaryOp.f(a, b)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  def withStackVerification(op: OpCode, stateIn: ProgramState, stateOut: ProgramState)(body: => Any): Any = {
    if (stateIn.stack.size < op.delta)
      stateOut shouldEqual stateIn.withError(StackUnderflow).halt
    else if (stateIn.stack.size > stateIn.stack.maxSize - op.alpha)
      stateOut shouldEqual stateIn.withError(StackOverflow).halt
    else {
      val expectedStackSize = stateIn.stack.size - op.delta + op.alpha
      stateOut.stack.size shouldEqual expectedStackSize

      body
    }
  }

  test(STOP) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)
      stateOut.halted shouldBe true
      stateIn shouldEqual stateOut.copy(halted = stateIn.halted)
    }
  }

  testBinaryOp(ADD)

  testBinaryOp(MUL)

  ignore("SUB") {
    // to be implemented
  }

  ignore("DIV") {
    // to be implemented
  }

  ignore("SDIV") {
    // to be implemented
  }

  ignore("MOD") {
    // to be implemented
  }

  ignore("SMOD") {
    // to be implemented
  }

  ignore("ADDMOD") {
    // to be implemented
  }

  ignore("MULMOD") {
    // to be implemented
  }

  ignore("EXP") {
    // to be implemented
  }

  ignore("SIGNEXTEND") {
    // to be implemented
  }

  ignore("LT") {
    // to be implemented
  }

  ignore("GT") {
    // to be implemented
  }

  ignore("SLT") {
    // to be implemented
  }

  ignore("SGT") {
    // to be implemented
  }

  ignore("EQ") {
    // to be implemented
  }

  ignore("ISZERO") {
    // to be implemented
  }

  ignore("AND") {
    // to be implemented
  }

  ignore("OR") {
    // to be implemented
  }

  ignore("XOR") {
    // to be implemented
  }

  ignore("NOT") {
    // to be implemented
  }

  ignore("BYTE") {
    // to be implemented
  }

  ignore("SHA3") {
    // to be implemented
  }

  ignore("ADDRESS") {
    // to be implemented
  }

  ignore("BALANCE") {
    // to be implemented
  }

  ignore("ORIGIN") {
    // to be implemented
  }

  ignore("CALLER") {
    // to be implemented
  }

  ignore("CALLVALUE") {
    // to be implemented
  }

  test(CALLDATALOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      callDataGen = getByteStringGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((offset, _)) = stateIn.stack.pop
        val Right((data, _)) = stateOut.stack.pop
        data shouldEqual stateIn.invoke.callData.slice(offset.intValue, offset.intValue + 32).padTo(32, 0.toByte)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  ignore("CALLDATASIZE") {
    // to be implemented
  }

  ignore("CALLDATACOPY") {
    // to be implemented
  }

  ignore("CODESIZE") {
    // to be implemented
  }

  ignore("CODECOPY") {
    // to be implemented
  }

  ignore("GASPRICE") {
    // to be implemented
  }

  ignore("EXTCODESIZE") {
    // to be implemented
  }

  ignore("EXTCODECOPY") {
    // to be implemented
  }

  ignore("BLOCKHASH") {
    // to be implemented
  }

  ignore("COINBASE") {
    // to be implemented
  }

  ignore("TIMESTAMP") {
    // to be implemented
  }

  ignore("NUMBER") {
    // to be implemented
  }

  ignore("DIFFICULTY") {
    // to be implemented
  }

  ignore("GASLIMIT") {
    // to be implemented
  }

  ignore("POP") {
    // to be implemented
  }

  ignore("MLOAD") {
    // to be implemented
  }

  ignore("MSTORE") {
    // to be implemented
  }

  ignore("MSTORE8") {
    // to be implemented
  }

  ignore("SLOAD") {
    // to be implemented
  }

  ignore("SSTORE") {
    // to be implemented
  }

  ignore("JUMP") {
    // to be implemented
  }

  ignore("JUMPI") {
    // to be implemented
  }

  ignore("PC") {
    // to be implemented
  }

  ignore("MSIZE") {
    // to be implemented
  }

  ignore("GAS") {
    // to be implemented
  }

  ignore("JUMPDEST") {
    // to be implemented
  }

  ignore("PUSH1") {
    // to be implemented
  }

  ignore("PUSH2") {
    // to be implemented
  }

  ignore("PUSH3") {
    // to be implemented
  }

  ignore("PUSH4") {
    // to be implemented
  }

  ignore("PUSH5") {
    // to be implemented
  }

  ignore("PUSH6") {
    // to be implemented
  }

  ignore("PUSH7") {
    // to be implemented
  }

  ignore("PUSH8") {
    // to be implemented
  }

  ignore("PUSH9") {
    // to be implemented
  }

  ignore("PUSH10") {
    // to be implemented
  }

  ignore("PUSH11") {
    // to be implemented
  }

  ignore("PUSH12") {
    // to be implemented
  }

  ignore("PUSH13") {
    // to be implemented
  }

  ignore("PUSH14") {
    // to be implemented
  }

  ignore("PUSH15") {
    // to be implemented
  }

  ignore("PUSH16") {
    // to be implemented
  }

  ignore("PUSH17") {
    // to be implemented
  }

  ignore("PUSH18") {
    // to be implemented
  }

  ignore("PUSH19") {
    // to be implemented
  }

  ignore("PUSH20") {
    // to be implemented
  }

  ignore("PUSH21") {
    // to be implemented
  }

  ignore("PUSH22") {
    // to be implemented
  }

  ignore("PUSH23") {
    // to be implemented
  }

  ignore("PUSH24") {
    // to be implemented
  }

  ignore("PUSH25") {
    // to be implemented
  }

  ignore("PUSH26") {
    // to be implemented
  }

  ignore("PUSH27") {
    // to be implemented
  }

  ignore("PUSH28") {
    // to be implemented
  }

  ignore("PUSH29") {
    // to be implemented
  }

  ignore("PUSH30") {
    // to be implemented
  }

  ignore("PUSH31") {
    // to be implemented
  }

  ignore("PUSH32") {
    // to be implemented
  }

  test("DUPi") {
    val dupTable = Table("DUPi opcodes", OpCode.opcodes.collect { case dup: DupOp => dup }: _*)

    forAll(dupTable) { op =>
      forAll(getProgramStateGen()) { stateIn =>
        val stateOut = op.execute(stateIn)

        withStackVerification(op, stateIn, stateOut) {
          val Right(expectedStack) = stateIn.stack.dup(op.i)
          val expectedState = stateIn.withStack(expectedStack).step()
          stateOut shouldEqual expectedState
        }
      }
    }
  }

  ignore("SWAP1") {
    // to be implemented
  }

  ignore("SWAP2") {
    // to be implemented
  }

  ignore("SWAP3") {
    // to be implemented
  }

  ignore("SWAP4") {
    // to be implemented
  }

  ignore("SWAP5") {
    // to be implemented
  }

  ignore("SWAP6") {
    // to be implemented
  }

  ignore("SWAP7") {
    // to be implemented
  }

  ignore("SWAP8") {
    // to be implemented
  }

  ignore("SWAP9") {
    // to be implemented
  }

  ignore("SWAP10") {
    // to be implemented
  }

  ignore("SWAP11") {
    // to be implemented
  }

  ignore("SWAP12") {
    // to be implemented
  }

  ignore("SWAP13") {
    // to be implemented
  }

  ignore("SWAP14") {
    // to be implemented
  }

  ignore("SWAP15") {
    // to be implemented
  }

  ignore("SWAP16") {
    // to be implemented
  }

  ignore("LOG0") {
    // to be implemented
  }

  ignore("LOG1") {
    // to be implemented
  }

  ignore("LOG2") {
    // to be implemented
  }

  ignore("LOG3") {
    // to be implemented
  }

  ignore("LOG4") {
    // to be implemented
  }

  ignore("CREATE") {
    // to be implemented
  }

  ignore("CALL") {
    // to be implemented
  }

  ignore("CALLCODE") {
    // to be implemented
  }

  test(RETURN) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      memGen = getMemoryGen(maxSize = 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(offset, size), _)) = stateIn.stack.pop(2)
        val (data, mem1) = stateIn.memory.load(offset, size)
        mem1.size should be >= (offset + size).intValue

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(mem1).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  ignore("DELEGATECALL") {
    // to be implemented
  }

  ignore("SUICIDE") {
    // to be implemented
  }

}
