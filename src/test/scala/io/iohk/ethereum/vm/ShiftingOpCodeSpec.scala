package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{ empty => bEmpty }
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{ Account, Address, BlockHeader, UInt256 }
import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState.{ PC, PS, TestVM }
import org.scalatest.prop.{ PropertyChecks, TableFor4 }
import org.scalatest.{ Matchers, WordSpec }

class ShiftingOpCodeSpec extends WordSpec with Matchers with PropertyChecks {

  // scalastyle:off magic.number

  val shiftLeft = Assembly(PUSH1, 2).code
  val shiftLeftEquivalent = Assembly(PUSH1, 2, EXP, MUL)

  val logicalShiftRight = Assembly(PUSH1, 2, SHR)
  val logicalShiftRightEquivalent = Assembly(PUSH1, 2, EXP, DIV)

  val arithmeticShiftRight = Assembly(PUSH1, 2, SAR)
  val notArithmeticShiftRightEquivalent = Assembly(PUSH1, 2, EXP, SDIV)

  val byteCode_0x80 = Array(1.toByte) ++ Array.fill(255)(0.toByte)
  val byteCode_0xff = Array.fill(256)(1.toByte)
  val byteCode_0xfe = Array.fill(255)(1.toByte) ++ Array(0.toByte)
  val byteCode_0x7f = Array.fill(255)(1.toByte)

  val byteString_0x40 = ByteString("0x4000000000000000000000000000000000000000000000000000000000000000")
  val byteString_0x07f = ByteString("0x000000000000000000000000000000000000000000000000000000000000007f")
  val byteString_0xfe = ByteString("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe")
  val byteString_0x7f = ByteString("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
  val byteString_0x80 = ByteString("0x8000000000000000000000000000000000000000000000000000000000000000")
  val byteString_0xff = ByteString("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

  val byteString_0x01 = 0x0000000000000000000000000000000000000000000000000000000000000001
  val byteString_0x00 = 0x0000000000000000000000000000000000000000000000000000000000000000

  // shift left
  // todo change it to 0x0 (without ByteString constructor)?
  val SHL1 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x00)
  val SHL1Result = byteString_0x01

  val SHL2 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x01, SHL)
  val byteString_0x002 = ByteString("0x0000000000000000000000000000000000000000000000000000000000000002")
  val SHL2Result = byteString_0x002

  val SHL3 = Assembly(PUSH1, byteString_0x01, PUSH1, 0xff, SHL)
  val SHL3Result = byteString_0x80

  val SHL4 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x0100, SHL)
  val SHL4Result = byteString_0x00

  val SHL5 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x0101, SHL)
  val SHL5Result = byteString_0x00

  val SHL6 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x00, SHL)
  val SHL6Result: ByteString = byteString_0xff

  val SHL7 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x01, SHL)
  val SHL7Result = byteString_0xfe

  val SHL8 = Assembly(PUSH1, byteString_0xff, PUSH1, 0xff, SHL)
  val SHL8Result = byteString_0x80

  val SHL9 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100, SHL)
  val SHL9Result = byteString_0x00

  val SHL10 = Assembly(PUSH1, byteString_0x00, PUSH1, 0x01, SHL)
  val SHL10Result = byteString_0x00

  val SHL11 = Assembly(PUSH1, byteString_0x07f, PUSH1, 0x01, SHL)
  val SHL11Result = byteString_0xfe

  // whole table of SHL test cases
  val smallValuesSHLTable: TableFor4[ByteString, Int, Int, Array[Byte]] = Table(("code", "stackArg1", "stackArg2", "result"),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, Array(1.toByte)),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, Array(2.toByte)),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0xff).code, byteString_0x01, 0xff, byteCode_0x80),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x0100).code, byteString_0x01, 0x0100, Array(0.toByte)),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x0101).code, byteString_0x01, 0x0101, Array(0.toByte)),
    (Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, Array(0.toByte))
  )

  val bigValuesSHLTable: TableFor4[ByteString, Array[Byte], Int, Array[Byte]] = Table(("code", "stackArg1", "stackArg2", "result"),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteCode_0xff, 0x00, byteCode_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteCode_0xff, 0x01, byteCode_0xfe),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteCode_0xff, 0xff, byteCode_0x80),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteCode_0xff, 0x0100, byteCode_0x80),
    (Assembly(PUSH1, byteString_0x07f, PUSH1, 0x01).code, byteCode_0xff, 0x01, byteCode_0x80)
  )

  // shift right (logical)
  val SHR1 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x00, SHR)
  val SHR1Result = byteString_0x01

  val SHR2 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x01, SHR)
  val SHR2Result = byteString_0x00

  val SHR3 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x01, SHR)
  val SHR3Result = byteString_0x40

  val SHR4 = Assembly(PUSH1, byteString_0x80, PUSH1, 0xff, SHR)
  val SHR4Result = byteString_0x01

  val SHR5 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100, SHR)
  val SHR5Result = byteString_0x00

  val SHR6 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101, SHR)
  val SHR6Result = byteString_0x00

  val SHR7 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x00, SHR)
  val SHR7Result = byteString_0xff

  val SHR8 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x01, SHR)
  val SHR8Result = byteString_0x7f

  val SHR9 = Assembly(PUSH1, byteString_0xff, PUSH1, 0xff, SHR)
  val SHR9Result = byteString_0x01

  val SHR10 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100, SHR)
  val SHR10Result = byteString_0x00

  val SHR11 = Assembly(PUSH1, byteString_0x00, PUSH1, 0x01, SHR)
  val SHR11Result = byteString_0x00

  // shift right (arithmetic)
  val SAR1 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x00, SAR)
  val SAR1Result = byteString_0x01

  val SAR2 = Assembly(PUSH1, byteString_0x01, PUSH1, 0x01, SAR)
  val SAR2Result = byteString_0x00

  val SAR3 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x01, SAR)
  val SAR3Result = ByteString("0xc000000000000000000000000000000000000000000000000000000000000000")

  val SAR4 = Assembly(PUSH1, byteString_0x80, PUSH1, 0xff, SAR)
  val SAR4Result = byteString_0xff

  val SAR5 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100, SAR)
  val SAR5Result = byteString_0xff

  val SAR6 = Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101, SAR)
  val SAR6Result: ByteString = byteString_0xff

  val SAR7 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x00, SAR)
  val SAR7Result = byteString_0xff

  val SAR8 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x01, SAR)
  val SAR8Result = byteString_0xff

  val SAR9 = Assembly(PUSH1, byteString_0xff, PUSH1, 0xff, SAR)
  val SAR9Result = byteString_0xff

  val SAR10 = Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100, SAR)
  val SAR10Result = byteString_0xff

  val SAR11 = Assembly(PUSH1, byteString_0x00, PUSH1, 0x01, SAR)
  val SAR11Result = byteString_0x00

  val SAR12 = Assembly(PUSH1, byteString_0x40, PUSH1, 0xfe, SAR)
  val SAR12Result = byteString_0x07f

  val byteString7ff = byteString_0x7f
  val SAR13 = Assembly(PUSH1, byteString7ff, PUSH1, 0xf8, SAR)
  val SAR13Result = byteString_0x07f

  val SAR14 = Assembly(PUSH1, byteString7ff, PUSH1, 0xfe, SAR)
  val SAR14Result = byteString_0x01

  val SAR15 = Assembly(PUSH1, byteString7ff, PUSH1, 0xff, SAR)
  val SAR15Result = byteString_0x00

  val SAR16 = Assembly(PUSH1, byteString7ff, PUSH1, 0x100, SAR)
  val SAR16Result = byteString_0x00

  "Shift opcodes" when {

    "calling a program that executes a shifting opcodes" should {
      "execute SHL opcodes" in new TestSetup {

        smallValuesSHLTable.foreach{ case (assemblyCode, arg1, arg2, expectedResult) =>
          val newWorld = defaultWorld
            .saveAccount(senderAddr, accountWithCode(assemblyCode))
            .saveCode(senderAddr, assemblyCode)

          val context: PC = getContext(newWorld)
          val env = ExecEnv(context, ByteString.empty, context.originAddr)

          val initMemory = Memory.empty.store(UInt256.Zero, assemblyCode)
          val initStack: Seq[UInt256] = Seq(UInt256(arg1), UInt256(arg2)).reverse

          val state = ProgramState(vm, context, env)
            .withStack(Stack.empty().push(initStack))
            .withMemory(initMemory)

          val result = SHL.execute(state)
          // todo: fix failing
          result.stack.pop._1.toBytes shouldBe expectedResult
        }
      }

      "test case" in new TestSetup {
        val EXAMPLE = Assembly(PUSH1, byteString_0x00, PUSH1, byteString_0x01)
        val code = EXAMPLE.code


        val newWorld = defaultWorld.saveAccount(senderAddr, accountWithCode(code)).saveCode(senderAddr, code)

        val context: PC = getContext(newWorld)
        val env = ExecEnv(context, ByteString.empty, context.originAddr)

        val inputData = code
        val initMemory = Memory.empty.store(UInt256.Zero, inputData)

        val initStack: Seq[UInt256] = Seq(UInt256(4), UInt256(1)).reverse
        val state = ProgramState(vm, context, env).withStack(Stack.empty().push(initStack)).withMemory(initMemory)

        // todo this one do NOT change state.stack but we want it to
        // find a way to interpret memory so it loads values on stack as declared in EXAMPLE
        state.memory.load(UInt256(0), code.size)

        val result: PS = SHL.execute(state)
        result.stack.pop._1 shouldBe 16
      }
    }

    "SHL" should {
      "equal PUSH1 2 EXP MUL" in new TestSetup {

      }
    }

    "SHR" should {
      "equal PUSH1 2 EXP DIV" in new TestSetup {

      }
    }

    "SAR" should {
      "not equal PUSH1 2 EXP SDIV" in {

      }
    }

  }

  trait TestSetup {
    val config = EvmConfig.ConstantinopleConfigBuilder(blockchainConfig)
    val vm = new TestVM

    val senderAddr = Address(0xcafebabeL)
    val senderAcc = Account(nonce = 1, balance = 1000000)

    val accountWithCode: ByteString => Account = code => Account.empty().withCode(kec256(code))

    def defaultWorld: MockWorldState = MockWorldState().saveAccount(senderAddr, senderAcc)

    val blockHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = 1,
      gasLimit = 10000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    def getContext(world: MockWorldState = defaultWorld, inputData: ByteString = bEmpty): PC =
      ProgramContext(
        callerAddr = senderAddr,
        originAddr = senderAddr,
        recipientAddr = None,
        gasPrice = 1,
        startGas = 1000000,
        inputData = inputData,
        value = 100,
        endowment = 100,
        doTransfer = true,
        blockHeader = blockHeader,
        callDepth = 0,
        world = world,
        initialAddressesToDelete = Set(),
        evmConfig = config
      )
  }

}
