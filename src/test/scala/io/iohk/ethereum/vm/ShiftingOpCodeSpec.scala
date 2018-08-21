package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{ empty => bEmpty }
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{ Account, Address, BlockHeader, UInt256 }
import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState.{ PC, TestVM }
import org.bouncycastle.util.encoders.Hex
import org.scalatest.prop.{ PropertyChecks, TableFor4 }
import org.scalatest.{ Matchers, WordSpec }

// scalastyle:off magic.number
class ShiftingOpCodeSpec extends WordSpec with Matchers with PropertyChecks {

  val array_0x01 = Array(1.toByte)
  val array_0x0 = Array(0.toByte)

  val byteCode_0x80: Array[Byte] = array_0x01 ++ Array.fill(255)(0.toByte)
  val byteCode_0xff: Array[Byte] = Array.fill(256)(1.toByte)
  val byteCode_0xfe: Array[Byte] = Array.fill(255)(1.toByte) ++ array_0x0
  val byteCode_0x7f: Array[Byte] = Array.fill(255)(1.toByte)

  val byteString_0x40 = ByteString(Hex.decode("4000000000000000000000000000000000000000000000000000000000000000"))
  val byteString_0x07f = ByteString(Hex.decode("000000000000000000000000000000000000000000000000000000000000007f"))
  val byteString_0xfe = ByteString(Hex.decode("fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe"))
  val byteString_0x7f = ByteString(Hex.decode("7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
  val byteString_0x80 = ByteString(Hex.decode("8000000000000000000000000000000000000000000000000000000000000000"))
  val byteString_0xff = ByteString(Hex.decode("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
  val byteString_0xc0 = ByteString(Hex.decode("c000000000000000000000000000000000000000000000000000000000000000"))
  val byteString_0x01 = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000001"))
  val byteString_0x00 = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000"))

  val array_0x80: Array[Byte] = byteString_0x80.toArray
  val array_0xff: Array[Byte] = byteString_0xff.toArray
  val array_0x07f: Array[Byte] = byteString_0x07f.toArray

  // shift left
  val SHLTable: TableFor4[ByteString, ByteString, Int, Array[Byte]] = Table(("code", "stackArg1", "stackArg2", "result"),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, Array(2.toByte)),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0xff).code, byteString_0x01, 0xff, array_0x80),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x0100).code, byteString_0x01, 0x0100, array_0x0),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x0101).code, byteString_0x01, 0x0101, array_0x0),
    (Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x0),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, byteString_0xfe.toArray),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0x80),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0x80),
    (Assembly(PUSH1, byteString_0x07f, PUSH1, 0x01).code, byteString_0x07f, 0x01, array_0x80)
  )

  // shift right (logical)
  val SHRTable: TableFor4[ByteString, ByteString, Int, Array[Byte]] = Table(("code", "stackArg1", "stackArg2", "result"),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x0),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0xff).code, byteString_0x80, 0xff, byteString_0x40.toArray),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0xff).code, byteString_0x80, 0xff, array_0x01),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100).code, byteString_0x80, 0x0100, array_0x0),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101).code, byteString_0x80, 0x0101, array_0x0),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, byteString_0x7f.toArray),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0x01),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0x0),
    (Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x0)
  )

  // shift right (arithmetic)
  val SARTable = Table(("code", "stackArg1", "stackArg2", "result"),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, array_0x0),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0x01).code, byteString_0x80, 0xff, byteString_0xc0.toArray),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0xff).code, byteString_0x80, 0xff, array_0xff),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100).code, byteString_0x80, 0x0100, array_0xff),
    (Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101).code, byteString_0x80, 0x0101, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0xff),
    (Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0xff),
    (Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x0),
    (Assembly(PUSH1, byteString_0x40, PUSH1, 0xfe).code, byteString_0x40, 0xfe, array_0x01),
    (Assembly(PUSH1, byteString_0x7f, PUSH1, 0xf8).code, byteString_0x7f, 0xf8, array_0x07f),
    (Assembly(PUSH1, byteString_0x7f, PUSH1, 0xfe).code, byteString_0x7f, 0xfe, array_0x01),
    (Assembly(PUSH1, byteString_0x7f, PUSH1, 0xff).code, byteString_0x7f, 0xff, array_0x0),
    (Assembly(PUSH1, byteString_0x7f, PUSH1, 0x100).code, byteString_0x7f, 0x100, array_0x0)
  )

  "Shift OpCodes" when {

    "calling a program that executes a shifting opcodes" should {

      SHLTable.foreach{ case (assemblyCode, arg1, arg2, expectedResult) =>
        s"execute SHL opcode on assembly code $assemblyCode with expected result $expectedResult" in new TestSetup {
          val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

          val result: ProgramState[MockWorldState, MockStorage] = SHL.execute(state)
          result.stack.pop._1.toSign.toByteArray shouldBe expectedResult
        }
      }

      SHRTable.foreach{ case (assemblyCode, arg1, arg2, expectedResult) =>
        s"execute SHR opcode on assembly code $assemblyCode with expected result $expectedResult" in new TestSetup {
          val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

          val result: ProgramState[MockWorldState, MockStorage] = SHL.execute(state)
          result.stack.pop._1.toSign.toByteArray shouldBe expectedResult
        }
      }

      SARTable.foreach{ case (assemblyCode, arg1, arg2, expectedResult) =>
        s"execute SAR opcode on args: $arg1, $arg2 with expected result $expectedResult" in new TestSetup {
          val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

          val result: ProgramState[MockWorldState, MockStorage] = SHL.execute(state)
          result.stack.pop._1.toChar shouldBe expectedResult
        }
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

    def prepareProgramState(assemblyCode: ByteString, arg1: ByteString, arg2: Int): ProgramState[MockWorldState, MockStorage] = {
      val newWorld = defaultWorld
        .saveAccount(senderAddr, accountWithCode(assemblyCode))
        .saveCode(senderAddr, assemblyCode)

      val context: PC = getContext(newWorld)
      val env = ExecEnv(context, ByteString.empty, context.originAddr)

      val initMemory = Memory.empty.store(UInt256.Zero, assemblyCode)
      val initStack: Seq[UInt256] = Seq(UInt256(arg1), UInt256(arg2))

      ProgramState(vm, context, env)
        .withStack(Stack.empty().push(initStack))
        .withMemory(initMemory)
    }
  }

}
