package io.iohk.ethereum.vm

import io.iohk.ethereum.crypto.sha3
import io.iohk.ethereum.vm.Generators._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class OpCodeSpecs extends FunSuite with Matchers with PropertyChecks {

  val binaryOps = OpCode.opcodes.collect { case op: BinaryOp => op }
  val unaryOps = OpCode.opcodes.collect { case op: UnaryOp => op }
  val pushOps = OpCode.opcodes.collect { case op: PushOp => op }
  val dupOps = OpCode.opcodes.collect { case op: DupOp => op }
  val swapOps = OpCode.opcodes.collect { case op: SwapOp => op }
  val logOps = OpCode.opcodes.collect { case op: LogOp => op }

  def test[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => test(op.toString)(f(op)))

  def ignore[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => ignore(op.toString)(f(op)))

  def withStackVerification(op: OpCode, stateIn: ProgramState, stateOut: ProgramState)(body: => Any): Any = {
    if (stateIn.stack.size < op.delta)
      stateOut shouldEqual stateIn.withError(StackUnderflow).halt
    else if (stateIn.stack.size - op.delta + op.alpha > stateIn.stack.maxSize)
      stateOut shouldEqual stateIn.withError(StackOverflow).halt
    else {
      val expectedStackSize = stateIn.stack.size - op.delta + op.alpha
      stateOut.stack.size shouldEqual expectedStackSize

      val Right((_, stack1)) = stateIn.stack.pop(op.delta)
      val Right((_, stack2)) = stateOut.stack.pop(op.alpha)
      stack1 shouldEqual stack2

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

  test(binaryOps: _*) { binaryOp =>
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

  test(unaryOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((a, _)) = stateIn.stack.pop
        val Right((result, _)) = stateOut.stack.pop
        result shouldEqual op.f(a)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
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

  ignore("SIGNEXTEND") {
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

  ignore("OR") {
    // to be implemented
  }

  ignore("XOR") {
    // to be implemented
  }

  ignore("BYTE") {
    // to be implemented
  }

  test(SHA3) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      memGen = getMemoryGen(maxSize = 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(offset, size), _)) = stateIn.stack.pop(2)
        val (data, mem1) = stateIn.memory.load(offset, size)
        val Right((result, _)) = stateOut.stack.pop
        result shouldEqual DataWord(sha3(data.toArray))

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(mem1).step()
        stateOut shouldEqual expectedState
      }
    }
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

  test(CALLVALUE) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((data, _)) = stateOut.stack.pop
        data shouldEqual DataWord(stateIn.context.callValue)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(CALLDATALOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      callDataGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((offset, _)) = stateIn.stack.pop
        val Right((data, _)) = stateOut.stack.pop
        data shouldEqual DataWord(stateIn.context.callData.slice(offset.intValue, offset.intValue + 32).padTo(32, 0.toByte))

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

  test(CODECOPY) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      memGen = getMemoryGen(256),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(memOffset, codeOffset, size), _)) = stateIn.stack.pop(3)
        val code = stateIn.program.getBytes(codeOffset.intValue, size.intValue)
        val (storedInMem, _) = stateOut.memory.load(memOffset, size)
        code shouldEqual storedInMem

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  ignore("GASPRICE") {
    // to be implemented
  }

  ignore("EXTCODESIZE") {
    // to be implemented
  }

  ignore(EXTCODECOPY) { op =>
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

  test(POP) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        stateOut shouldEqual stateIn.withStack(stateOut.stack).step()
      }
    }
  }

  test(MLOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((addr, _)) = stateIn.stack.pop
        val Right((result, _)) = stateOut.stack.pop
        val (data, _) = stateIn.memory.load(addr)
        result shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
      }
    }
  }

  test(MSTORE) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = DataWord(256)),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(addr, value), _)) = stateIn.stack.pop(2)
        val (data, _) = stateOut.memory.load(addr)
        value shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
      }
    }
  }

  ignore("MSTORE8") {
    // to be implemented
  }

  test(SLOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(DataWord(256)),
      storageGen = getStorageGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((addr, _)) = stateIn.stack.pop
        val data = stateIn.storage.load(addr)
        val Right((result, _)) = stateOut.stack.pop
        result shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).step()
      }
    }
  }

  test(SSTORE) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(DataWord(256)),
      storageGen = getStorageGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(addr, value), _)) = stateIn.stack.pop(2)
        val data = stateOut.storage.load(addr)
        data shouldEqual value

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withStorage(stateOut.storage).step()
      }
    }
  }

  test(JUMP) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((pos, _)) = stateIn.stack.pop
        stateOut shouldEqual stateIn.withStack(stateOut.stack).goto(pos.intValue)
      }
    }
  }

  test(JUMPI) { op =>
    val stateGen = getProgramStateGen(
      // FIXME perhaps there's a better way to make sure there are some zeros
      stackGen = getStackGen(maxWord = DataWord(2))
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right((Seq(pos, cond), _)) = stateIn.stack.pop(2)
        val expectedState =
          if (cond != 0)
            stateIn.withStack(stateOut.stack).goto(pos.intValue)
          else
            stateIn.withStack(stateOut.stack).step()

        stateOut shouldEqual expectedState
      }
    }
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

  test(JUMPDEST) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        stateOut shouldEqual stateIn.step()
      }
    }
  }

  test(pushOps: _*) { op =>
    val stateGen = getProgramStateGen(codeGen = getByteStringGen(0, 32))

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val bytes = stateIn.program.getBytes(stateIn.pc + 1, op.i + 1)
        val Right(expectedStack) = stateIn.stack.push(DataWord(bytes))
        val expectedState = stateIn.withStack(expectedStack).step(op.i + 2)
        stateOut shouldEqual expectedState
      }
    }
  }

  test(dupOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right(expectedStack) = stateIn.stack.dup(op.i)
        val expectedState = stateIn.withStack(expectedStack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(swapOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = op.execute(stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val Right(expectedStack) = stateIn.stack.swap(op.i + 1)
        val expectedState = stateIn.withStack(expectedStack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  ignore(logOps: _*) { op =>
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

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(mem1).withReturnData(data).halt
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
