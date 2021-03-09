package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState.PC
import io.iohk.ethereum.vm.MockWorldState.TestVM
import org.bouncycastle.util.encoders.Hex
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor5
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// scalastyle:off magic.number
class ShiftingOpCodeSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  val array_0x01 = Array(1.toByte)
  val array_0x00 = Array(0.toByte)

  val byteCode_0x80: Array[Byte] = array_0x01 ++ Array.fill(255)(0.toByte)
  val byteCode_0xff: Array[Byte] = Array.fill(256)(1.toByte)
  val byteCode_0xfe: Array[Byte] = Array.fill(255)(1.toByte) ++ array_0x00
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
  val array_0xfe: Array[Byte] = byteString_0xfe.toArray

  // shift left
  val SHLTable: TableFor5[Int, ByteString, ByteString, Int, Array[Byte]] = Table(
    ("number", "code", "stackArg1", "stackArg2", "result"),
    (1, Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (2, Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, Array(2.toByte)),
    (3, Assembly(PUSH1, byteString_0x01, PUSH1, 0xff).code, byteString_0x01, 0xff, array_0x80),
    (4, Assembly(PUSH1, byteString_0x01, PUSH1, 0x0100).code, byteString_0x01, 0x0100, array_0x00),
    (5, Assembly(PUSH1, byteString_0x01, PUSH1, 0x0101).code, byteString_0x01, 0x0101, array_0x00),
    (6, Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (7, Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, array_0xfe),
    (8, Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0x80),
    (9, Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0x00),
    (10, Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x00),
    (11, Assembly(PUSH1, byteString_0x7f, PUSH1, 0x01).code, byteString_0x7f, 0x01, array_0xfe)
  )

  // shift right (logical)
  val SHRTable: TableFor5[Int, ByteString, ByteString, Int, Array[Byte]] = Table(
    ("number", "code", "stackArg1", "stackArg2", "result"),
    (1, Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (2, Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, array_0x00),
    (3, Assembly(PUSH1, byteString_0x80, PUSH1, 0x01).code, byteString_0x80, 0x01, byteString_0x40.toArray),
    (4, Assembly(PUSH1, byteString_0x80, PUSH1, 0xff).code, byteString_0x80, 0xff, array_0x01),
    (5, Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100).code, byteString_0x80, 0x0100, array_0x00),
    (6, Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101).code, byteString_0x80, 0x0101, array_0x00),
    (7, Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (8, Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, byteString_0x7f.toArray),
    (9, Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0x01),
    (10, Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0x00),
    (11, Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x00)
  )

  // shift right (arithmetic)
  val SARTable: TableFor5[Int, ByteString, ByteString, Int, Array[Byte]] = Table(
    ("number", "code", "stackArg1", "stackArg2", "result"),
    (1, Assembly(PUSH1, byteString_0x01, PUSH1, 0x00).code, byteString_0x01, 0x00, array_0x01),
    (2, Assembly(PUSH1, byteString_0x01, PUSH1, 0x01).code, byteString_0x01, 0x01, array_0x00),
    (3, Assembly(PUSH1, byteString_0x80, PUSH1, 0x01).code, byteString_0x80, 0x01, byteString_0xc0.toArray),
    (4, Assembly(PUSH1, byteString_0x80, PUSH1, 0xff).code, byteString_0x80, 0xff, array_0xff),
    (5, Assembly(PUSH1, byteString_0x80, PUSH1, 0x0100).code, byteString_0x80, 0x0100, array_0xff),
    (6, Assembly(PUSH1, byteString_0x80, PUSH1, 0x0101).code, byteString_0x80, 0x0101, array_0xff),
    (7, Assembly(PUSH1, byteString_0xff, PUSH1, 0x00).code, byteString_0xff, 0x00, array_0xff),
    (8, Assembly(PUSH1, byteString_0xff, PUSH1, 0x01).code, byteString_0xff, 0x01, array_0xff),
    (9, Assembly(PUSH1, byteString_0xff, PUSH1, 0xff).code, byteString_0xff, 0xff, array_0xff),
    (10, Assembly(PUSH1, byteString_0xff, PUSH1, 0x0100).code, byteString_0xff, 0x0100, array_0xff),
    (11, Assembly(PUSH1, byteString_0x00, PUSH1, 0x01).code, byteString_0x00, 0x01, array_0x00),
    (12, Assembly(PUSH1, byteString_0x40, PUSH1, 0xfe).code, byteString_0x40, 0xfe, array_0x01),
    (13, Assembly(PUSH1, byteString_0x7f, PUSH1, 0xf8).code, byteString_0x7f, 0xf8, array_0x07f),
    (14, Assembly(PUSH1, byteString_0x7f, PUSH1, 0xfe).code, byteString_0x7f, 0xfe, array_0x01),
    (15, Assembly(PUSH1, byteString_0x7f, PUSH1, 0xff).code, byteString_0x7f, 0xff, array_0x00),
    (16, Assembly(PUSH1, byteString_0x7f, PUSH1, 0x100).code, byteString_0x7f, 0x100, array_0x00),
    (17, Assembly(PUSH1, byteString_0x00, PUSH1, 0x0101).code, byteString_0x00, 0x0101, array_0x00)
  )

  "Shift OpCodes" when {

    "calling a program that executes a shifting opcodes" should {

      SHLTable.foreach { case (index, assemblyCode, arg1, arg2, expectedResult) =>
        s"execute $index test case for SHL opcode: arg=${Hex.toHexString(arg1.toArray)}, " +
          s"shift=${arg2.toHexString} with expected result ${Hex.toHexString(expectedResult)}" in new TestSetup {
            val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

            val result: ProgramState[MockWorldState, MockStorage] = SHL.execute(state)
            result.stack.pop._1 shouldBe UInt256(expectedResult)
          }
      }

      SHRTable.foreach { case (index, assemblyCode, arg1, arg2, expectedResult) =>
        s"execute $index test case for SHR opcode: arg=${Hex.toHexString(arg1.toArray)}, " +
          s"shift=${arg2.toHexString} with expected result ${Hex.toHexString(expectedResult)}" in new TestSetup {
            val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

            val result: ProgramState[MockWorldState, MockStorage] = SHR.execute(state)
            result.stack.pop._1 shouldBe UInt256(expectedResult)
          }
      }

      SARTable.foreach { case (index, assemblyCode, arg1, arg2, expectedResult) =>
        s"execute $index test case fo SAR opcode: arg=${Hex.toHexString(arg1.toArray)}, " +
          s"shift=${arg2.toHexString} with expected result ${Hex.toHexString(expectedResult)}" in new TestSetup {
            val state: ProgramState[MockWorldState, MockStorage] = prepareProgramState(assemblyCode, arg1, arg2)

            val result: ProgramState[MockWorldState, MockStorage] = SAR.execute(state)
            result.stack.pop._1 shouldBe UInt256(expectedResult)
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

    val blockHeader = BlockFixtures.ValidBlock.header.copy(
      difficulty = 1000000,
      number = 1,
      gasLimit = 10000000,
      gasUsed = 0,
      unixTimestamp = 0
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
        evmConfig = config,
        originalWorld = world
      )

    def prepareProgramState(
        assemblyCode: ByteString,
        arg1: ByteString,
        arg2: Int
    ): ProgramState[MockWorldState, MockStorage] = {
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
