package io.iohk.ethereum.vm

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import Generators._
import GasFee._


// scalastyle:off magic.number
class OpCodeGasSpec extends FunSuite with OpCodeTesting with Matchers with PropertyChecks {

  val stackOpsFees = (pushOps ++ dupOps ++ swapOps).map(_ -> G_verylow.value)

  val constGasFees = Map[OpCode, GasFee](
    STOP -> G_zero,
    ADD -> G_verylow,
    MUL -> G_low,
    SUB -> G_verylow,
    DIV -> G_low,
    LT -> G_verylow,
    EQ -> G_verylow,
    ISZERO -> G_verylow,
    AND -> G_verylow,
    NOT -> G_verylow,
    CALLVALUE -> G_base,
    CALLDATALOAD -> G_verylow,
    POP -> G_base,
    SLOAD -> G_sload,
    JUMP -> G_mid,
    JUMPI -> G_high,
    JUMPDEST -> G_jumpdest
  ).mapValues(_.value) ++ stackOpsFees

  def verifyGas(expectedGas: BigInt, stateIn: ProgramState, stateOut: ProgramState): Unit = {
    if (expectedGas > stateIn.gas)
      stateOut shouldEqual stateIn.withError(OutOfGas)
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
      (1024, 1023, 2, G_memory.value),
      (64000, 128000, 2000, G_memory.value * 2063 + 24430),
      (1, 1, 1, 0),
      (BigInt(2).pow(30), BigInt(2).pow(30) - 1, 1, 0),
      (BigInt(2).pow(30), BigInt(2).pow(30), 1, G_memory.value + (2 * BigInt(2).pow(25) + 1) / 512),
      (0, BigInt(2).pow(64), 1, DataWord.MaxValue),
      (4225664, 1, 8421505, G_memory.value * 4015841 + 89561265)
    )

    forAll(testData) { (memSize, addr, dataSize, expectedCost) =>
      calcMemCost(memSize, addr, dataSize) shouldEqual expectedCost
    }


    val big = getBigIntGen(max = BigInt(2).pow(32))
    forAll(big, big, big) { (memSize, addr, dataSize) =>

      val memNeeded: BigInt = if (dataSize > 0) addr + dataSize else 0

      def c(ms: BigInt): BigInt = {
        val a = wordsForBytes(ms)
        G_memory.value * a + a * a / 512
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
      gasGen = getBigIntGen(max = op.constGas.value * 2))

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      verifyGas(op.constGas.value, stateIn, stateOut)
    }
  }

  test(EXP) { op =>
    val maxGas = 100000000
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, m: DataWord), _) = stateIn.stack.pop(2)
      val expectedGas = G_exp.value + G_expbyte.value * wordsForBytes(m)

      verifyGas(expectedGas, stateIn, stateOut)
    }


    val table = Table[BigInt, BigInt](("exponent", "expectedGas"), (0,  1))
    forAll(table) { (m, expectedGas) =>
      //val stateIn = stateGen.filter()
    }
  }

  ignore(SHA3) { op =>
    val maxGas = 50000 //FIXME
    val memSize = 256
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, size), _) = stateIn.stack.pop(2)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val shaCost = G_sha3.value + G_sha3word.value * wordsForBytes(size)
      val expectedGas = memCost + shaCost

      verifyGas(expectedGas, stateIn, stateOut)
    }

  }

  test(CODECOPY) { op =>
    val maxGas = 1000
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
      val copyCost = G_copy.value * wordsForBytes(size)
      val expectedGas = G_verylow.value + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(EXTCODECOPY) { op =>
    val maxGas = 2000
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 4, maxWord = DataWord(256)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, addr, _, size), _) = stateIn.stack.pop(4)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val copyCost = G_copy.value * wordsForBytes(size)
      val expectedGas = G_extcode.value + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MLOAD) { op =>
    val maxGasUsage = 50000 //FIXME: twice actual max
    val memSize = 256
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (addr, _) = stateIn.stack.pop
      val expectedGas = G_verylow.value + calcMemCost(stateIn.memory.size, addr, DataWord.Size) //TODO: test calcMemCost separately

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE) { op =>
    val maxGasUsage = 50000 //FIXME: twice actual max
    val memSize = 256
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (addr, _) = stateIn.stack.pop
      val expectedGas = G_verylow.value + calcMemCost(stateIn.memory.size, addr, DataWord.Size) //TODO: test calcMemCost separately

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SSTORE) { op =>
    val maxGasUsage = 50000 //FIXME: twice actual max
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(2)),
      gasGen = getBigIntGen(max = maxGasUsage),
      storageGen = getStorageGen() //FIXME wrt zeroes
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, value), _) = stateIn.stack.pop(2)
      val oldValue = stateIn.storage.load(addr)
      val expectedGas = if (oldValue.isZero && !value.isZero) G_sset.value else G_sreset.value
      val expectedRefund = if (value.isZero && !oldValue.isZero) R_sclear.value else BigInt(0)

      verifyGas(expectedGas, stateIn, stateOut)

      if (expectedGas <= stateIn.gas) {
        stateOut.gasRefund shouldEqual (stateIn.gasRefund + expectedRefund)
      }
    }
  }

  test(logOps: _*) { op =>
    val maxGas = 500
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 6, maxWord = DataWord(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(addr, size, _*), _) = stateIn.stack.pop(op.delta)
      val memCost = calcMemCost(stateIn.memory.size, addr, size)
      val logCost = G_log.value + G_logdata.value * size + op.i * G_logtopic.value
      val expectedGas = G_log.value + memCost + logCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(RETURN) { op =>
    val maxGas = 500
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

  verifyAllOpCodesRegistered()
}
