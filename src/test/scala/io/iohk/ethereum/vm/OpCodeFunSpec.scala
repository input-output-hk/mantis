package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address, TxLogEntry}
import io.iohk.ethereum.vm.Generators._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class OpCodeFunSpec extends FunSuite with OpCodeTesting with Matchers with PropertyChecks {

  import MockWorldState.PS

  override val config = EvmConfig.HomesteadConfig

  def executeOp(op: OpCode, stateIn: PS): PS = {
    // gas is not tested in this spec
    op.execute(stateIn).copy(gas = stateIn.gas, gasRefund = stateIn.gasRefund)
  }

  def withStackVerification(op: OpCode, stateIn: PS, stateOut: PS)(body: => Any): Any = {
    if (stateIn.stack.size < op.delta)
      stateOut shouldEqual stateIn.withError(StackUnderflow).halt
    else if (stateIn.stack.size - op.delta + op.alpha > stateIn.stack.maxSize)
      stateOut shouldEqual stateIn.withError(StackOverflow).halt
    else {
      if (stateOut.error.isEmpty) {
        val expectedStackSize = stateIn.stack.size - op.delta + op.alpha
        stateOut.stack.size shouldEqual expectedStackSize

        val (_, stack1) = stateIn.stack.pop(op.delta)
        val (_, stack2) = stateOut.stack.pop(op.alpha)
        stack1 shouldEqual stack2
      }
      body
    }
  }

  def stateWithCode(state: PS, code: ByteString): PS = {
    val newProgram = Program(code)
    state.copy(context = state.context.copy(env = state.context.env.copy(program = newProgram)))
  }

  test(STOP) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)
      stateOut.halted shouldBe true
      stateIn shouldEqual stateOut.copy(halted = stateIn.halted)
    }
  }

  test(unaryOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (a, _) = stateIn.stack.pop
        val (result, _) = stateOut.stack.pop
        result shouldEqual op.f(a)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(binaryOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(a, b), _) = stateIn.stack.pop(2)
        val (result, _) = stateOut.stack.pop
        result shouldEqual op.f(a, b)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(ternaryOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(a, b, c), _) = stateIn.stack.pop(3)
        val (result, _) = stateOut.stack.pop
        result shouldEqual op.f(a, b, c)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(constOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (result, _) = stateOut.stack.pop
        result shouldEqual op.f(stateIn)

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(SHA3) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(maxSize = 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, size), _) = stateIn.stack.pop(2)
        val (data, mem1) = stateIn.memory.load(offset, size)
        val (result, _) = stateOut.stack.pop
        result shouldEqual UInt256(kec256(data.toArray))

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(mem1).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(BALANCE) { op =>
    forAll(getProgramStateGen(), getUInt256Gen()) { (stateIn, accountBalance) =>
      val stateOut = executeOp(op, stateIn)
      withStackVerification(op, stateIn, stateOut) {
        val (_, stack1) = stateIn.stack.pop
        stateOut shouldEqual stateIn.withStack(stack1.push(UInt256.Zero)).step()
      }

      val (addr, stack1) = stateIn.stack.pop

      val account = Account(balance = accountBalance)
      val world1 = stateIn.world.saveAccount(Address(addr mod UInt256(BigInt(2).pow(160))), account)

      val stateInWithAccount = stateIn.withWorld(world1)
      val stateOutWithAccount = executeOp(op, stateInWithAccount)

      withStackVerification(op, stateInWithAccount, stateOutWithAccount) {
        val stack2 = stack1.push(accountBalance)
        stateOutWithAccount shouldEqual stateInWithAccount.withStack(stack2).step()
      }
    }
  }

  test(CALLDATALOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      inputDataGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (offset, _) = stateIn.stack.pop
        val (data, _) = stateOut.stack.pop
        data shouldEqual UInt256(OpCode.sliceBytes(stateIn.inputData, offset.toInt, 32))

        val expectedState = stateIn.withStack(stateOut.stack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(CALLDATACOPY) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(256),
      inputDataGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(memOffset, dataOffset, size), _) = stateIn.stack.pop(3)
        val data = OpCode.sliceBytes(stateIn.inputData, dataOffset.toInt, size.toInt)
        val (storedInMem, _) = stateOut.memory.load(memOffset, size)
        data shouldEqual storedInMem

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(CODECOPY) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(256),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(memOffset, codeOffset, size), _) = stateIn.stack.pop(3)
        val code = stateIn.program.getBytes(codeOffset.toInt, size.toInt)
        val (storedInMem, _) = stateOut.memory.load(memOffset, size)
        code shouldEqual storedInMem

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(EXTCODESIZE) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256))
    )
    val codeGen = getByteStringGen(0, 512)

    forAll(stateGen, codeGen) { (stateIn, extCode) =>
      val stateOut = executeOp(op, stateIn)
      withStackVerification(op, stateIn, stateOut) {
        val (_, stack1) = stateIn.stack.pop
        stateOut shouldEqual stateIn.withStack(stack1.push(UInt256.Zero)).step()
      }

      val (addr, stack1) = stateIn.stack.pop
      val program = Program(extCode)
      val world1 = stateIn.world.saveCode(Address(addr), program.code)

      val stateInWithExtCode = stateIn.withWorld(world1)
      val stateOutWithExtCode = executeOp(op, stateInWithExtCode)

      withStackVerification(op, stateInWithExtCode, stateOutWithExtCode) {
        val stack2 = stack1.push(UInt256(extCode.size))
        stateOutWithExtCode shouldEqual stateInWithExtCode.withStack(stack2).step()
      }
    }
  }

  test(EXTCODECOPY) { op =>
    val stateGen: Gen[PS] = for {
      extCode <- getByteStringGen(0, 256)

      stateIn <- getProgramStateGen(
        stackGen = getStackGen(maxWord = UInt256(256)),
        memGen = getMemoryGen(256),
        codeGen = getByteStringGen(0, 256)
      )

      doSave <- Gen.oneOf(false, true, true)

      addr = Address(stateIn.stack.pop._1)
      hash = kec256(extCode)
      world = if (doSave) stateIn.world.saveAccount(addr, Account.Empty.copy(codeHash = hash)) else stateIn.world
    } yield stateIn.withWorld(world)

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(addr, memOffset, codeOffset, size), _) = stateIn.stack.pop(4)
        val code = OpCode.sliceBytes(stateIn.world.getCode(Address(addr)), codeOffset.toInt, size.toInt)
        val (storedInMem, _) = stateOut.memory.load(memOffset, size)
        code shouldEqual storedInMem

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(BLOCKHASH) { op =>
    val stateGen: Gen[PS] = for {
      stateIn <- getProgramStateGen(
        stackGen = getStackGen(maxWord = UInt256(512)),
        blockNumberGen = getUInt256Gen(0, 512)
      )
    } yield stateIn

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (blockHeaderNumber, stack1) = stateIn.stack.pop

        val withinLimits =
          stateIn.env.blockHeader.number - blockHeaderNumber.toBigInt <= 256 &&
          blockHeaderNumber.toBigInt < stateIn.env.blockHeader.number

        val hash = stateIn.world.getBlockHash(blockHeaderNumber).filter(_ => withinLimits).getOrElse(UInt256.Zero)

        val expectedState = stateIn.withStack(stack1.push(hash)).step()
        stateOut shouldBe expectedState
      }
    }
  }

  test(POP) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        stateOut shouldEqual stateIn.withStack(stateOut.stack).step()
      }
    }
  }

  test(MLOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (offset, _) = stateIn.stack.pop
        val (result, _) = stateOut.stack.pop
        val (data, _) = stateIn.memory.load(offset)
        result shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
      }
    }
  }

  test(MSTORE) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, value), _) = stateIn.stack.pop(2)
        val (data, _) = stateOut.memory.load(offset)
        value shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
      }
    }
  }

  test(MSTORE8) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, value), _) = stateIn.stack.pop(2)
        val (data, _) = stateOut.memory.load(offset, 1)
        ByteString((value mod 256).toByte) shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withMemory(stateOut.memory).step()
      }
    }
  }

  test(SLOAD) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(UInt256(256)),
      storageGen = getStorageGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (offset, _) = stateIn.stack.pop
        val data = stateIn.storage.load(offset)
        val (result, _) = stateOut.stack.pop
        result shouldEqual data

        stateOut shouldEqual stateIn.withStack(stateOut.stack).step()
      }
    }
  }

  test(SSTORE) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(UInt256(256)),
      storageGen = getStorageGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, value), _) = stateIn.stack.pop(2)
        val data = stateOut.storage.load(offset)
        data shouldEqual value

        stateOut shouldEqual stateIn.withStack(stateOut.stack).withStorage(stateOut.storage).step()
      }
    }
  }

  test(JUMP) { op =>
    val jumpDest: Byte = (Byte.MaxValue / 2).toByte
    val stateGen = getProgramStateGen(
      stackGen = getStackGen().map(stack => stack.pop._2.push(UInt256(jumpDest))),
      codeGen = getByteStringGen(0, Byte.MaxValue)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (pos, _) = stateIn.stack.pop
        if(stateIn.program.validJumpDestinations.contains(pos.intValue))
          stateOut shouldEqual stateIn.withStack(stateOut.stack).goto(pos.intValue)
        else
          stateOut shouldEqual stateIn.withError(InvalidJump(pos.intValue))
      }
    }

    val codeAllJumps = ByteString((0 to Byte.MaxValue).map(_ => JUMP.code).toArray)
    def newStateWithJumpDestination(jumpDestination: Int): PS = {
      val newState = getProgramStateGen().sample.get
      val newStack = newState.stack.pop._2.push(UInt256(jumpDestination))
      newState.copy(stack = newStack)
    }

    //Jump to valid destination
    val codeWithValidDestination = ByteString(codeAllJumps.toArray.updated(jumpDest, JUMPDEST.code))
    val stateInWithValidDestination = stateWithCode(newStateWithJumpDestination(jumpDest), codeWithValidDestination)
    val stateOutWithValidDestination = executeOp(op, stateInWithValidDestination)

    withStackVerification(op, stateInWithValidDestination, stateOutWithValidDestination) {
      val (pos, _) = stateInWithValidDestination.stack.pop
      stateOutWithValidDestination shouldEqual stateInWithValidDestination.withStack(stateOutWithValidDestination.stack).goto(pos.intValue)
    }

    //Jump to destination not a JUMPDEST
    val stateInWithInvalidDestination1 = stateWithCode(newStateWithJumpDestination(jumpDest), codeAllJumps)
    val stateOutWithInvalidDestination1 = executeOp(op, stateInWithInvalidDestination1)

    withStackVerification(op, stateInWithInvalidDestination1, stateOutWithInvalidDestination1) {
      stateOutWithInvalidDestination1 shouldEqual stateInWithInvalidDestination1.withError(InvalidJump(jumpDest.toInt))
    }

    //Jump to destination inside PUSH
    val jumpDestInsidePush = 16
    val codeWithInvalidDestination = ByteString(PUSH31.code +: (0 to 31).map(_ => 0.toByte).toArray)
    val stateInWithInvalidDestination2 = stateWithCode(newStateWithJumpDestination(jumpDestInsidePush), codeWithInvalidDestination)
    val stateOutWithInvalidDestination2 = executeOp(op, stateInWithInvalidDestination2)

    withStackVerification(op, stateInWithInvalidDestination2, stateOutWithInvalidDestination2) {
      stateOutWithInvalidDestination2 shouldEqual stateInWithInvalidDestination2.withError(InvalidJump(jumpDestInsidePush))
    }
  }

  test(JUMPI) { op =>
    val jumpDest: Byte = (Byte.MaxValue / 2).toByte
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 1, maxUInt = UInt256(1)).map(stack => stack.push(UInt256(jumpDest))),
      codeGen = getByteStringGen(0, Byte.MaxValue)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(pos, cond), _) = stateIn.stack.pop(2)
        val expectedState =
          if (!cond.isZero)
            if (stateIn.program.validJumpDestinations.contains(pos.intValue))
              stateIn.withStack(stateOut.stack).goto(pos.intValue)
            else
              stateIn.withError(InvalidJump(pos.intValue))
          else
            stateIn.withStack(stateOut.stack).step()

        stateOut shouldEqual expectedState
      }
    }

    val codeAllJumps = ByteString((0 to Byte.MaxValue).map(_ => JUMP.code).toArray)
    def newStateWithJumpDestination(jumpDestination: Int): PS = {
      val newState = getProgramStateGen().sample.get
      val newStack = newState.stack.pop(2)._2.push(UInt256(1)).push(UInt256(jumpDestination))
      newState.copy(stack = newStack)
    }

    //Jump to valid destination
    val codeWithValidDestination = ByteString(codeAllJumps.toArray.updated(jumpDest, JUMPDEST.code))
    val stateInWithValidDestination = stateWithCode(newStateWithJumpDestination(jumpDest), codeWithValidDestination)
    val stateOutWithValidDestination = executeOp(op, stateInWithValidDestination)

    withStackVerification(op, stateInWithValidDestination, stateOutWithValidDestination) {
      val (Seq(pos, _), _) = stateInWithValidDestination.stack.pop(2)
      stateOutWithValidDestination shouldEqual stateInWithValidDestination.withStack(stateOutWithValidDestination.stack).goto(pos.intValue)
    }

    //Jump to destination not a JUMPDEST
    val stateInWithInvalidDestination1 = stateWithCode(newStateWithJumpDestination(jumpDest), codeAllJumps)
    val stateOutWithInvalidDestination1 = executeOp(op, stateInWithInvalidDestination1)

    withStackVerification(op, stateInWithInvalidDestination1, stateOutWithInvalidDestination1) {
      stateOutWithInvalidDestination1 shouldEqual stateInWithInvalidDestination1.withError(InvalidJump(jumpDest.toInt))
    }

    //Jump to destination inside PUSH
    val jumpDestInsidePush = 16
    val codeWithInvalidDestination = ByteString(PUSH31.code +: (0 to 31).map(_ => 0.toByte).toArray)
    val stateInWithInvalidDestination2 = stateWithCode(newStateWithJumpDestination(jumpDestInsidePush), codeWithInvalidDestination)
    val stateOutWithInvalidDestination2 = executeOp(op, stateInWithInvalidDestination2)

    withStackVerification(op, stateInWithInvalidDestination2, stateOutWithInvalidDestination2) {
      stateOutWithInvalidDestination2 shouldEqual stateInWithInvalidDestination2.withError(InvalidJump(jumpDestInsidePush))
    }
  }

  test(JUMPDEST) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        stateOut shouldEqual stateIn.step()
      }
    }
  }

  test(pushOps: _*) { op =>
    val stateGen = getProgramStateGen(codeGen = getByteStringGen(0, 32))

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val bytes = stateIn.program.getBytes(stateIn.pc + 1, op.i + 1)
        val expectedStack = stateIn.stack.push(UInt256(bytes))
        val expectedState = stateIn.withStack(expectedStack).step(op.i + 2)
        stateOut shouldEqual expectedState
      }
    }
  }

  test(dupOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val expectedStack = stateIn.stack.dup(op.i)
        val expectedState = stateIn.withStack(expectedStack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(swapOps: _*) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val expectedStack = stateIn.stack.swap(op.i + 1)
        val expectedState = stateIn.withStack(expectedStack).step()
        stateOut shouldEqual expectedState
      }
    }
  }

  test(logOps: _*) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(maxSize = 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, size, topics @ _*), stack1) = stateIn.stack.pop(op.delta)
        val (data, mem1) = stateIn.memory.load(offset, size)
        val logEntry = TxLogEntry(stateIn.env.ownerAddr, topics.map(_.bytes), data)
        val expectedState = stateIn.withStack(stack1).withMemory(mem1).withLog(logEntry).step()

        logEntry.logTopics.size shouldEqual op.i
        stateOut shouldEqual expectedState
      }
    }
  }

  ignore("CREATE") {
    // to be implemented
  }

  test(RETURN) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(maxWord = UInt256(256)),
      memGen = getMemoryGen(maxSize = 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      withStackVerification(op, stateIn, stateOut) {
        val (Seq(offset, size), _) = stateIn.stack.pop(2)
        val (data, mem1) = stateIn.memory.load(offset, size)
        mem1.size should be >= (offset + size).toInt

        val expectedState = stateIn.withStack(stateOut.stack).withMemory(mem1).withReturnData(data).halt
        stateOut shouldEqual expectedState
      }
    }
  }

  test(INVALID) { op =>
    forAll(getProgramStateGen()) { stateIn =>
      val stateOut = executeOp(op, stateIn)

      val expectedState = stateIn.withError(InvalidOpCode(op.code))
      stateOut shouldEqual expectedState
    }
  }

  test(SELFDESTRUCT) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2)
    )
    forAll(stateGen) { stateIn =>
      val stateOut = executeOp(op, stateIn)
      withStackVerification(op, stateIn, stateOut) {
        val (refundDW, stack1) = stateIn.stack.pop
        val world1 = stateIn.world
          .transfer(stateIn.ownAddress, Address(refundDW), stateIn.ownBalance)
          .saveAccount(Address(refundDW), Account.Empty)
        val expectedState = stateIn
          .withWorld(world1)
          .withAddressToDelete(stateIn.context.env.ownerAddr)
          .withStack(stack1)
          .halt
        stateOut shouldEqual expectedState
      }
    }
  }

  verifyAllOpCodesRegistered(except = CALL, CALLCODE, DELEGATECALL)

}

