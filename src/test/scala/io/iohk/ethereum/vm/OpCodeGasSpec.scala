package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.FeeSchedule.GasCost._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import Generators._
import GasFee._
import UInt256.{One, Two, Zero}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm.MockWorldState.PS
import scala.language.implicitConversions

// scalastyle:off magic.number
class OpCodeGasSpec extends FunSuite with OpCodeTesting with Matchers with PropertyChecks {

  override val config = EvmConfig.FrontierConfig

  implicit def scheduleKeyToUInt256(key: FeeSchedule.GasCost): UInt256 = config.feeSchedule(key)

  val stackOpsFees = (pushOps ++ dupOps ++ swapOps).map(_ -> config.feeSchedule(G_verylow))
  val constOpsFees = constOps.map(_ -> config.feeSchedule(G_base))

  val constGasFees = Map[OpCode, FeeSchedule.GasCost](
    STOP -> G_zero,
    ADD -> G_verylow,
    MUL -> G_low,
    SUB -> G_verylow,
    DIV -> G_low,
    SDIV -> G_low,
    MOD -> G_low,
    SMOD -> G_low,
    ADDMOD -> G_mid,
    MULMOD -> G_mid,
    SIGNEXTEND -> G_low,
    LT -> G_verylow,
    GT -> G_verylow,
    SLT -> G_verylow,
    SGT -> G_verylow,
    EQ -> G_verylow,
    ISZERO -> G_verylow,
    AND -> G_verylow,
    OR -> G_verylow,
    XOR -> G_verylow,
    NOT -> G_verylow,
    BYTE -> G_verylow,
    ADDRESS -> G_base,
    BALANCE -> G_balance,
    CALLVALUE -> G_base,
    CALLDATALOAD -> G_verylow,
    CALLDATASIZE -> G_base,
    EXTCODESIZE -> G_extcodesize,
    BLOCKHASH -> G_blockhash,
    COINBASE -> G_base,
    TIMESTAMP -> G_base,
    NUMBER -> G_base,
    DIFFICULTY -> G_base,
    GASLIMIT -> G_base,
    POP -> G_base,
    SLOAD -> G_sload,
    JUMP -> G_mid,
    JUMPI -> G_high,
    GAS -> G_base,
    JUMPDEST -> G_jumpdest
  ).mapValues(config.feeSchedule.apply) ++ stackOpsFees ++ constOpsFees

  def verifyGas(expectedGas: UInt256, stateIn: PS, stateOut: PS, allowOOG: Boolean = true): Unit = {
    if (stateOut.error.contains(OutOfGas) && allowOOG)
      stateIn.gas should be < expectedGas
    else if (stateOut.error.contains(OutOfGas) && !allowOOG)
      fail(s"Unexpected $OutOfGas error")
    else if (stateOut.error.isDefined && stateOut.error.collect{ case InvalidJump(dest) => dest }.isEmpty)
      //Found error that is not an InvalidJump
      fail(s"Unexpected ${stateOut.error.get} error")
    else
      stateOut.gas shouldEqual (stateIn.gas - expectedGas)
  }

  test("wordsForBytes helper") {
    val testData = Table(("bytes", "words"),
      0 -> 0, 1 -> 1, 2 -> 1, 32 -> 1, 33 -> 2, 64 -> 2, 65 -> 3, 256 -> 8, 257 -> 9)

    forAll(testData) { (argument, expectedResult) =>
      wordsForBytes(argument) shouldEqual expectedResult
    }
  }

  test("calcMemCost helper") {
    val testData = Table[UInt256, UInt256, UInt256, UInt256](
      ("memSize", "offset", "dataSize", "expectedCost"),
      (0, 0, 0, 0),
      (256, 128, 128, 0),
      (128, 1024, 0, 0),
      (123, 122, 1, 0),
      (1024, 1023, 2, G_memory),
      (64000, 128000, 2000, G_memory * 2063 + 24430),
      (1, 1, 1, 0),
      (Two ** 30, Two ** 30 - 1, 1, 0),
      (Two ** 30, Two ** 30, 1, G_memory + (Two ** 25 * 2 + 1) / 512),
      (0, Two ** 64, 1, UInt256.MaxValue / 2),
      (4225664, 1, 8421505, G_memory * 4015841 + 89561265)
    )

    forAll(testData) { (memSize, offset, dataSize, expectedCost) =>
      calcMemCost(memSize, offset, dataSize, config) shouldEqual expectedCost
    }


    val uint = getUInt256Gen(max = Two ** 32)
    forAll(uint, uint, uint) { (memSize, offset, dataSize) =>

      val memNeeded: UInt256 = if (dataSize > 0) offset + dataSize else 0

      def c(ms: UInt256): UInt256 = {
        val a = wordsForBytes(ms)
        G_memory * a + a * a / 512
      }

      val expectedCost: UInt256 =
        if (memNeeded > config.maxMemory)
          UInt256.MaxValue / 2
        else if (memSize > memNeeded)
          0
        else
          c(memNeeded) - c(memSize)

      calcMemCost(memSize, offset, dataSize, config) shouldEqual expectedCost
    }
  }

  test(constGasOps: _*) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = op.delta),
      gasGen = getUInt256Gen(max = op.constGasScheduleKey * 2))

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      verifyGas(constGasFees(op), stateIn, stateOut)
    }
  }

  test(EXP) { op =>
    val table = Table[UInt256, UInt256](("exponent", "expectedGas"),
      (0,  G_exp),
      (1, G_exp + G_expbyte),
      (255, G_exp + G_expbyte),
      (256, G_exp + G_expbyte * 2),
      (Two ** 248 - 1, G_exp + G_expbyte * 31),
      (Two ** 248, G_exp + G_expbyte * 32),
      (UInt256.MaxValue, G_exp + G_expbyte * 32)
    )

    forAll(table) { (m, expectedGas) =>
      val stackIn = Stack.empty().push(m).push(Zero)
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_exp + G_expbyte * 32
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2),
      gasGen = getUInt256Gen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, m: UInt256), _) = stateIn.stack.pop(2)
      val expectedGas = G_exp + G_expbyte * m.byteSize

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SHA3) { op =>
    val table = Table[UInt256, UInt256](("size", "expectedGas"),
      (0,  G_sha3),
      (1, G_sha3 + G_sha3word * 1),
      (32, G_sha3 + G_sha3word * 1),
      (33, G_sha3 + G_sha3word * 2),
      (Two ** 16, G_sha3 + G_sha3word * 2048),
      (Two ** 16 + 1, G_sha3 + G_sha3word * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(size).push(Zero)
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val memSize = 256
    val maxGas = G_sha3 + G_sha3word * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getUInt256Gen(max = maxGas),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size), _) = stateIn.stack.pop(2)
      val memCost = calcMemCost(stateIn.memory.size, offset, size, config)
      val shaCost = G_sha3 + G_sha3word * wordsForBytes(size)
      val expectedGas = memCost + shaCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CALLDATACOPY) { op =>
    val table = Table[UInt256, UInt256](("size", "expectedGas"),
      (0,  G_verylow),
      (1, G_verylow + G_copy * 1),
      (32, G_verylow + G_copy * 1),
      (33, G_verylow + G_copy * 2),
      (Two ** 16, G_verylow + G_copy * 2048),
      (Two ** 16 + 1, G_verylow + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(size).push(Zero).push(Zero)
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 3, maxUInt = UInt256(256)),
      memGen = getMemoryGen(256),
      gasGen = getUInt256Gen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, _, size), _) = stateIn.stack.pop(3)
      val memCost = calcMemCost(stateIn.memory.size, offset, size, config)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CODECOPY) { op =>
    val table = Table[UInt256, UInt256](("size", "expectedGas"),
      (0,  G_verylow),
      (1, G_verylow + G_copy * 1),
      (32, G_verylow + G_copy * 1),
      (33, G_verylow + G_copy * 2),
      (Two ** 16, G_verylow + G_copy * 2048),
      (Two ** 16 + 1, G_verylow + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(size).push(Zero).push(Zero)
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 3, maxUInt = UInt256(256)),
      memGen = getMemoryGen(256),
      gasGen = getUInt256Gen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, _, size), _) = stateIn.stack.pop(3)
      val memCost = calcMemCost(stateIn.memory.size, offset, size, config)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(EXTCODECOPY) { op =>
    val table = Table[UInt256, UInt256](("size", "expectedGas"),
      (0,  G_extcodecopy_base),
      (1, G_extcodecopy_base + G_copy * 1),
      (32, G_extcodecopy_base + G_copy * 1),
      (33, G_extcodecopy_base + G_copy * 2),
      (Two ** 16, G_extcodecopy_base + G_copy * 2048),
      (Two ** 16 + 1, G_extcodecopy_base + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(Seq(size, Zero, Zero, Zero))
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 4, maxUInt = UInt256(256)),
      gasGen = getUInt256Gen(max = maxGas),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, offset, _, size), _) = stateIn.stack.pop(4)
      val memCost = calcMemCost(stateIn.memory.size, offset, size, config)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_extcode + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MLOAD) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + calcMemCost(memSize, memSize, memSize, config)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getUInt256Gen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + calcMemCost(stateIn.memory.size, offset, UInt256.Size, config)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + calcMemCost(memSize, memSize, memSize, config)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getUInt256Gen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + calcMemCost(stateIn.memory.size, offset, UInt256.Size, config)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE8) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + calcMemCost(memSize, memSize, memSize, config)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getUInt256Gen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + calcMemCost(stateIn.memory.size, offset, 1, config)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SSTORE) { op =>
    val storage = MockStorage.Empty.store(Zero, One)
    val table = Table[UInt256, UInt256, UInt256, UInt256](("offset", "value", "expectedGas", "expectedRefund"),
      (0, 1, G_sreset, 0),
      (0, 0, G_sreset, R_sclear),
      (1, 0, G_sreset, 0),
      (1, 1, G_sset, 0)
    )

    forAll(table) { (offset, value, expectedGas, expectedRefund) =>
      val stackIn = Stack.empty().push(value).push(offset)
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withStorage(storage).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGasUsage = G_sset + G_sreset
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = Two),
      gasGen = getUInt256Gen(max = maxGasUsage),
      storageGen = getStorageGen(3, getUInt256Gen(max = One))
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, value), _) = stateIn.stack.pop(2)
      val oldValue = stateIn.storage.load(offset)
      val expectedGas: UInt256 = if (oldValue.isZero && !value.isZero) G_sset else G_sreset
      val expectedRefund: UInt256 = if (value.isZero && !oldValue.isZero) R_sclear else Zero

      verifyGas(expectedGas, stateIn, stateOut)

      if (expectedGas <= stateIn.gas) {
        stateOut.gasRefund shouldEqual (stateIn.gasRefund + expectedRefund)
      }
    }
  }

  test(logOps: _*) { op =>
    val table = Table[UInt256, UInt256](("size", "expectedGas"),
      (0, G_log + G_logtopic * op.i),
      (13, G_log + G_logtopic * op.i + G_logdata * 13)
    )

    forAll(table) { (size, expectedGas) =>
      val topics = Seq.fill(op.delta - 2)(Zero)
      val stackIn = Stack.empty().push(topics).push(size).push(Zero)
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGas = G_log + G_logdata * 256 + G_logtopic * 4 + calcMemCost(256, 256, 256, config)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 6, maxUInt = UInt256(256)),
      gasGen = getUInt256Gen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size, _*), _) = stateIn.stack.pop(op.delta)
      val memCost = calcMemCost(stateIn.memory.size, offset, size, config)
      val logCost: UInt256 = G_logdata * size + op.i * scheduleKeyToUInt256(G_logtopic)
      val expectedGas: UInt256 = G_log + memCost + logCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(RETURN) { op =>
    val maxGas = calcMemCost(256, 256, 256, config)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(256)),
      gasGen = getUInt256Gen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size), _) = stateIn.stack.pop(2)
      val expectedGas = calcMemCost(stateIn.memory.size, offset, size, config)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SELFDESTRUCT) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2)
    )

    // Sending refund to a non-existent account
    forAll(stateGen) { stateIn =>
      val (Seq(_, refund), _) = stateIn.stack.pop(2)
      whenever(stateIn.world.getAccount(Address(refund)).isEmpty) {
        val stateOut = op.execute(stateIn)
        stateOut.gasRefund shouldEqual scheduleKeyToUInt256(R_selfdestruct)
        verifyGas(G_selfdestruct + G_selfdestruct_to_new_account, stateIn, stateOut)
      }
    }

    // Sending refund to an already existing account
    forAll(stateGen) { (stateIn) =>
      val (Seq(_, refund), _) = stateIn.stack.pop(2)
      val world = stateIn.world.saveAccount(
        Address(refund),
        Account.Empty)
      val updatedStateIn = stateIn.withWorld(world)
      val stateOut = op.execute(updatedStateIn)
      verifyGas(G_selfdestruct, updatedStateIn, stateOut)
      stateOut.gasRefund shouldEqual scheduleKeyToUInt256(R_selfdestruct)
    }

    // Owner account was already selfdestructed
    forAll(stateGen) { stateIn =>
      val (Seq(_, refund), _) = stateIn.stack.pop(2)
      whenever(stateIn.world.getAccount(Address(refund)).isEmpty) {
        val updatedStateIn = stateIn.withAddressToDelete(stateIn.context.env.ownerAddr)
        val stateOut = op.execute(updatedStateIn)
        verifyGas(G_selfdestruct + G_selfdestruct_to_new_account, updatedStateIn, stateOut)
        stateOut.gasRefund shouldEqual 0
      }
    }

  }

  verifyAllOpCodesRegistered(except = CALL, CALLCODE, DELEGATECALL, INVALID)
}
