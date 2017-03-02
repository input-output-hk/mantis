package io.iohk.ethereum.vm

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import Generators._
import GasFee._


// scalastyle:off magic.number
class OpCodeGasSpec extends FunSuite with OpCodeTesting with Matchers with PropertyChecks {

  val stackOpsFees = (pushOps ++ dupOps ++ swapOps).map(_ -> G_verylow)
  val constOpsFees = constOps.map(_ -> G_base)

  val constGasFees = Map[OpCode, BigInt](
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
    CALLVALUE -> G_base,
    CALLDATALOAD -> G_verylow,
    CALLDATASIZE -> G_base,
    EXTCODESIZE -> G_extcode,
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
  ) ++ stackOpsFees ++ constOpsFees

  def verifyGas(expectedGas: BigInt, stateIn: ProgramState, stateOut: ProgramState, allowOOG: Boolean = true): Unit = {
    if (stateOut.error.contains(OutOfGas) && allowOOG)
      stateIn.gas should be < expectedGas
    else if (stateOut.error.contains(OutOfGas) && !allowOOG)
      fail(s"Unexpected $OutOfGas error")
    else if (stateOut.error.isDefined)
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
    val testData = Table[BigInt, BigInt, BigInt, BigInt](
      ("memSize", "address", "dataSize", "expectedCost"),
      (0, 0, 0, 0),
      (256, 128, 128, 0),
      (128, 1024, 0, 0),
      (123, 122, 1, 0),
      (1024, 1023, 2, G_memory),
      (64000, 128000, 2000, G_memory * 2063 + 24430),
      (1, 1, 1, 0),
      (BigInt(2).pow(30), BigInt(2).pow(30) - 1, 1, 0),
      (BigInt(2).pow(30), BigInt(2).pow(30), 1, G_memory + (2 * BigInt(2).pow(25) + 1) / 512),
      (0, BigInt(2).pow(64), 1, DataWord.MaxValue),
      (4225664, 1, 8421505, G_memory * 4015841 + 89561265)
    )

    forAll(testData) { (memSize, addr, dataSize, expectedCost) =>
      calcMemCost(memSize, addr, dataSize) shouldEqual expectedCost
    }


    val big = getBigIntGen(max = BigInt(2).pow(32))
    forAll(big, big, big) { (memSize, addr, dataSize) =>

      val memNeeded: BigInt = if (dataSize > 0) addr + dataSize else 0

      def c(ms: BigInt): BigInt = {
        val a = wordsForBytes(ms)
        G_memory * a + a * a / 512
      }

      val expectedCost: BigInt =
        if (memNeeded > MaxMemory)
          DataWord.MaxValue
        else if (memSize > memNeeded)
          0
        else
          c(memNeeded) - c(memSize)

      calcMemCost(memSize, addr, dataSize) shouldEqual expectedCost
    }
  }

  test(constGasOps: _*) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = op.delta),
      gasGen = getBigIntGen(max = op.constGas * 2))

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      verifyGas(constGasFees(op), stateIn, stateOut)
    }
  }

  test(EXP) { op =>
    val table = Table[BigInt, BigInt](("exponent", "expectedGas"),
      (0,  G_exp),
      (1, G_exp + G_expbyte),
      (255, G_exp + G_expbyte),
      (256, G_exp + G_expbyte * 2),
      (BigInt(2).pow(248) - 1, G_exp + G_expbyte * 31),
      (BigInt(2).pow(248), G_exp + G_expbyte * 32),
      (DataWord.MaxValue, G_exp + G_expbyte * 32)
    )

    forAll(table) { (m, expectedGas) =>
      val stackIn = Stack.empty().push(DataWord(m)).push(DataWord.Zero)
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_exp + G_expbyte * 32
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, m: DataWord), _) = stateIn.stack.pop(2)
      val expectedGas = G_exp + G_expbyte * m.byteSize

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SHA3) { op =>
    val table = Table[BigInt, BigInt](("size", "expectedGas"),
      (0,  G_sha3),
      (1, G_sha3 + G_sha3word * 1),
      (32, G_sha3 + G_sha3word * 1),
      (33, G_sha3 + G_sha3word * 2),
      (BigInt(2).pow(16), G_sha3 + G_sha3word * 2048),
      (BigInt(2).pow(16) + 1, G_sha3 + G_sha3word * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(DataWord(size)).push(DataWord.Zero)
      val memIn = Memory.empty.store(DataWord.Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val memSize = 256
    val maxGas = G_sha3 + G_sha3word * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, size), _) = stateIn.stack.pop(2)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val shaCost = G_sha3 + G_sha3word * wordsForBytes(size)
      val expectedGas = memCost + shaCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CALLDATACOPY) { op =>
    val table = Table[BigInt, BigInt](("size", "expectedGas"),
      (0,  G_verylow),
      (1, G_verylow + G_copy * 1),
      (32, G_verylow + G_copy * 1),
      (33, G_verylow + G_copy * 2),
      (BigInt(2).pow(16), G_verylow + G_copy * 2048),
      (BigInt(2).pow(16) + 1, G_verylow + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(DataWord(size)).push(DataWord.Zero).push(DataWord.Zero)
      val memIn = Memory.empty.store(DataWord.Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 3, maxWord = DataWord(256)),
      memGen = getMemoryGen(256),
      gasGen = getBigIntGen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, _, size), _) = stateIn.stack.pop(3)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CODECOPY) { op =>
    val table = Table[BigInt, BigInt](("size", "expectedGas"),
      (0,  G_verylow),
      (1, G_verylow + G_copy * 1),
      (32, G_verylow + G_copy * 1),
      (33, G_verylow + G_copy * 2),
      (BigInt(2).pow(16), G_verylow + G_copy * 2048),
      (BigInt(2).pow(16) + 1, G_verylow + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(DataWord(size)).push(DataWord.Zero).push(DataWord.Zero)
      val memIn = Memory.empty.store(DataWord.Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 3, maxWord = DataWord(256)),
      memGen = getMemoryGen(256),
      gasGen = getBigIntGen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, _, size), _) = stateIn.stack.pop(3)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(EXTCODECOPY) { op =>
    val table = Table[BigInt, BigInt](("size", "expectedGas"),
      (0,  G_extcode),
      (1, G_extcode + G_copy * 1),
      (32, G_extcode + G_copy * 1),
      (33, G_extcode + G_copy * 2),
      (BigInt(2).pow(16), G_extcode + G_copy * 2048),
      (BigInt(2).pow(16) + 1, G_extcode + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(Seq(DataWord(size), DataWord.Zero, DataWord.Zero, DataWord.Zero))
      val memIn = Memory.empty.store(DataWord.Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGas = G_verylow + G_copy * 8
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 4, maxWord = DataWord(256)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, addr, _, size), _) = stateIn.stack.pop(4)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_extcode + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MLOAD) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + calcMemCost(memSize, memSize, memSize)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (addr, _) = stateIn.stack.pop
      val expectedGas = G_verylow + calcMemCost(stateIn.memory.size, addr, DataWord.Size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + calcMemCost(memSize, memSize, memSize)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (addr, _) = stateIn.stack.pop
      val expectedGas = G_verylow + calcMemCost(stateIn.memory.size, addr, DataWord.Size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SSTORE) { op =>
    val storage = Storage.Empty.store(DataWord.Zero, DataWord(1))
    val table = Table[BigInt, BigInt, BigInt, BigInt](("addr", "value", "expectedGas", "expectedRefund"),
      (0, 1, G_sreset, 0),
      (0, 0, G_sreset, R_sclear),
      (1, 0, G_sreset, 0),
      (1, 1, G_sset, 0)
    )

    forAll(table) { (addr, value, expectedGas, expectedRefund) =>
      val stackIn = Stack.empty().push(DataWord(value)).push(DataWord(addr))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withStorage(storage).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }


    val maxGasUsage = G_sset + G_sreset
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(2)),
      gasGen = getBigIntGen(max = maxGasUsage),
      storageGen = getStorageGen(3, getDataWordGen(max = DataWord(1)))
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, value), _) = stateIn.stack.pop(2)
      val oldValue = stateIn.storage.load(addr)
      val expectedGas = if (oldValue.isZero && !value.isZero) G_sset else G_sreset
      val expectedRefund = if (value.isZero && !oldValue.isZero) R_sclear else BigInt(0)

      verifyGas(expectedGas, stateIn, stateOut)

      if (expectedGas <= stateIn.gas) {
        stateOut.gasRefund shouldEqual (stateIn.gasRefund + expectedRefund)
      }
    }
  }

  test(logOps: _*) { op =>
    val table = Table[BigInt, BigInt](("size", "expectedGas"),
      (0, G_log + G_logtopic * op.i),
      (13, G_log + G_logtopic * op.i + G_logdata * 13)
    )

    forAll(table) { (size, expectedGas) =>
      val topics = Seq.fill(op.delta - 2)(DataWord.Zero)
      val stackIn = Stack.empty().push(topics).push(DataWord(size)).push(DataWord.Zero)
      val memIn = Memory.empty.store(DataWord.Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGas = G_log + G_logdata * 256 + G_logtopic * 4 + calcMemCost(256, 256, 256)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 6, maxWord = DataWord(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, size, _*), _) = stateIn.stack.pop(op.delta)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val logCost = G_logdata * size + op.i * G_logtopic
      val expectedGas = G_log + memCost + logCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(RETURN) { op =>
    val maxGas = calcMemCost(256, 256, 256)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, size), _) = stateIn.stack.pop(2)
      val expectedGas = calcMemCost(stateIn.memory.size, addr, size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  verifyAllOpCodesRegistered(except = CALL, CALLCODE, DELEGATECALL, INVALID)
}
