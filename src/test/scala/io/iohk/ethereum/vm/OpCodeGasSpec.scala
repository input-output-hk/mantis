package io.iohk.ethereum.vm

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.domain.UInt256._
import io.iohk.ethereum.vm.Generators._

import Fixtures.blockchainConfig

class OpCodeGasSpec extends AnyFunSuite with OpCodeTesting with Matchers with ScalaCheckPropertyChecks {

  override val config: EvmConfig = EvmConfig.PhoenixConfigBuilder(blockchainConfig)

  import config.feeSchedule._

  val stackOpsFees: List[(OpCode with ConstGas, BigInt)] = (pushOps ++ dupOps ++ swapOps).map(_ -> G_verylow)
  val constOpsFees: List[(ConstOp, BigInt)] = constOps.map(_ -> G_base)

  val constGasFees: Map[OpCode, BigInt] = Map[OpCode, BigInt](
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
    EXTCODEHASH -> G_balance,
    EXTCODESIZE -> G_extcode,
    CALLVALUE -> G_base,
    CALLDATALOAD -> G_verylow,
    CALLDATASIZE -> G_base,
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
    JUMPDEST -> G_jumpdest,
    SHL -> G_verylow,
    SHR -> G_verylow,
    SAR -> G_verylow,
    SELFBALANCE -> G_low
  ) ++ stackOpsFees ++ constOpsFees

  test("wordsForBytes helper") {
    val testData =
      Table(("bytes", "words"), 0 -> 0, 1 -> 1, 2 -> 1, 32 -> 1, 33 -> 2, 64 -> 2, 65 -> 3, 256 -> 8, 257 -> 9)

    forAll(testData) { (argument, expectedResult) =>
      wordsForBytes(argument) shouldEqual expectedResult
    }
  }

  test("calcMemCost helper") {
    val testData = Table[UInt256, UInt256, UInt256, BigInt](
      ("memSize", "offset", "dataSize", "expectedCost"),
      (0, 0, 0, 0),
      (0, -15, 32, UInt256.MaxValue / 2),
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
      config.calcMemCost(memSize, offset, dataSize) shouldEqual expectedCost
    }

    val uint = getUInt256Gen(max = Two ** 32)
    forAll(uint, uint, uint) { (memSize, offset, dataSize) =>
      val memNeeded: UInt256 = if (dataSize > 0) offset + dataSize else 0

      def c(ms: UInt256): BigInt = {
        val a = wordsForBytes(ms)
        G_memory * a + a * a / 512
      }

      val expectedCost: BigInt =
        if (memNeeded > EvmConfig.MaxMemory)
          UInt256.MaxValue / 2
        else if (memSize > memNeeded)
          0
        else
          c(memNeeded) - c(memSize)

      config.calcMemCost(memSize, offset, dataSize) shouldEqual expectedCost
    }
  }

  test(constGasOps: _*) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = op.delta),
      gasGen = getBigIntGen(max = op.baseGasFn(config.feeSchedule) * 2)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      verifyGas(constGasFees(op), stateIn, stateOut)
    }
  }

  test(EXP) { op =>
    val table = Table[UInt256, BigInt](
      ("exponent", "expectedGas"),
      (0, G_exp),
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
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, m: UInt256), _) = stateIn.stack.pop(2)
      val expectedGas = G_exp + G_expbyte * m.byteSize

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SHA3) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
      (0, G_sha3),
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
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size), _) = stateIn.stack.pop(2)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val shaCost = G_sha3 + G_sha3word * wordsForBytes(size)
      val expectedGas = memCost + shaCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CALLDATACOPY) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
      (0, G_verylow),
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
      gasGen = getBigIntGen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, _, size), _) = stateIn.stack.pop(3)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(RETURNDATACOPY) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
      (0, G_verylow),
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
      gasGen = getBigIntGen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, _, size), _) = stateIn.stack.pop(3)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(CODECOPY) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
      (0, G_verylow),
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
      gasGen = getBigIntGen(max = maxGas),
      codeGen = getByteStringGen(0, 256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, _, size), _) = stateIn.stack.pop(3)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_verylow + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(EXTCODECOPY) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
      (0, G_extcode),
      (1, G_extcode + G_copy * 1),
      (32, G_extcode + G_copy * 1),
      (33, G_extcode + G_copy * 2),
      (Two ** 16, G_extcode + G_copy * 2048),
      (Two ** 16 + 1, G_extcode + G_copy * 2049)
    )

    forAll(table) { (size, expectedGas) =>
      val stackIn = Stack.empty().push(Seq(size, Zero, Zero, Zero))
      val memIn = Memory.empty.store(Zero, Array.fill[Byte](size.toInt)(-1))
      val stateIn = getProgramStateGen().sample.get.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGas = 2 * (G_extcode + G_copy * 8)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 4, maxUInt = UInt256(256)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(_, offset, _, size), _) = stateIn.stack.pop(4)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas = G_extcode + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MLOAD) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + config.calcMemCost(memSize, memSize, memSize)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + config.calcMemCost(stateIn.memory.size, offset, UInt256.Size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + config.calcMemCost(memSize, memSize, memSize)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + config.calcMemCost(stateIn.memory.size, offset, UInt256.Size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(MSTORE8) { op =>
    val memSize = 256
    val maxGasUsage = G_verylow + config.calcMemCost(memSize, memSize, memSize)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(memSize)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (offset, _) = stateIn.stack.pop
      val expectedGas = G_verylow + config.calcMemCost(stateIn.memory.size, offset, 1)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SSTORE) { op =>
    // Before Constantinople + Petersburg
    // Constantinople + Phoenix tested in SSTOREOpCodeGasPostConstantinopleSpec

    val petersburgConfig = EvmConfig.PetersburgConfigBuilder(blockchainConfig)
    import petersburgConfig.feeSchedule._

    val storage = MockStorage.Empty.store(Zero, One)
    val table = Table[UInt256, UInt256, BigInt, BigInt](
      ("offset", "value", "expectedGas", "expectedRefund"),
      (0, 1, G_sreset, 0),
      (0, 0, G_sreset, R_sclear),
      (1, 0, G_sreset, 0),
      (1, 1, G_sset, 0)
    )

    forAll(table) { (offset, value, expectedGas, _) =>
      val stackIn = Stack.empty().push(value).push(offset)
      val stateIn = getProgramStateGen(
        blockNumberGen = Gen.frequency(
          (1, getUInt256Gen(0, Fixtures.ConstantinopleBlockNumber - 1)),
          (1, getUInt256Gen(Fixtures.PetersburgBlockNumber + 1, Fixtures.PhoenixBlockNumber - 1))
        ),
        evmConfig = petersburgConfig,
        isTopHeader = true
      ).sample.get.withStack(stackIn).withStorage(storage).copy(gas = expectedGas)
      val stateOut = op.execute(stateIn)
      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
    }

    val maxGasUsage = G_sset + G_sreset
    val stateGen = getProgramStateGen(
      blockNumberGen = Gen.frequency(
        (1, getUInt256Gen(0, Fixtures.ConstantinopleBlockNumber - 1)),
        (1, getUInt256Gen(Fixtures.PetersburgBlockNumber + 1, Fixtures.PhoenixBlockNumber - 1))
      ),
      stackGen = getStackGen(elems = 2, maxUInt = Two),
      gasGen = getBigIntGen(max = maxGasUsage),
      storageGen = getStorageGen(3, getUInt256Gen(max = One)),
      evmConfig = petersburgConfig,
      isTopHeader = true
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, value), _) = stateIn.stack.pop(2)
      val oldValue = stateIn.storage.load(offset)
      val expectedGas: BigInt = if (UInt256(oldValue).isZero && !value.isZero) G_sset else G_sreset
      val expectedRefund: BigInt = if (value.isZero && !UInt256(oldValue).isZero) R_sclear else Zero

      verifyGas(expectedGas, stateIn, stateOut)

      if (expectedGas <= stateIn.gas) {
        stateOut.gasRefund shouldEqual (stateIn.gasRefund + expectedRefund)
      }
    }
  }

  test(logOps: _*) { op =>
    val table = Table[UInt256, BigInt](
      ("size", "expectedGas"),
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

    val maxGas = G_log + G_logdata * 256 + G_logtopic * 4 + config.calcMemCost(256, 256, 256)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 6, maxUInt = UInt256(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size, _*), _) = stateIn.stack.pop(op.delta)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val logCost = G_logdata * size + op.i * G_logtopic
      val expectedGas: BigInt = G_log + memCost + logCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(RETURN) { op =>
    val maxGas = config.calcMemCost(256, 256, 256)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size), _) = stateIn.stack.pop(2)
      val expectedGas = config.calcMemCost(stateIn.memory.size, offset, size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(REVERT) { op =>
    val maxGas = config.calcMemCost(256, 256, 256)
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxUInt = UInt256(256)),
      gasGen = getBigIntGen(max = maxGas)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(offset, size), _) = stateIn.stack.pop(2)
      val expectedGas = config.calcMemCost(stateIn.memory.size, offset, size)

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SELFDESTRUCT) { op =>
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 1),
      evmConfig = EvmConfig.PostEIP160ConfigBuilder(blockchainConfig)
    )

    // Sending refund to a non-existent account
    forAll(stateGen) { stateIn =>
      val (refund, _) = stateIn.stack.pop
      whenever(stateIn.world.getAccount(Address(refund)).isEmpty) {
        val stateOut = op.execute(stateIn)
        stateOut.gasRefund shouldEqual R_selfdestruct
        verifyGas(G_selfdestruct + G_newaccount, stateIn, stateOut)
      }
    }

    // Sending refund to an already existing account
    forAll(stateGen) { stateIn =>
      val (refund, _) = stateIn.stack.pop
      val world = stateIn.world.saveAccount(Address(refund), Account.empty())
      val updatedStateIn = stateIn.withWorld(world)
      val stateOut = op.execute(updatedStateIn)
      verifyGas(G_selfdestruct, updatedStateIn, stateOut)
      stateOut.gasRefund shouldEqual R_selfdestruct
    }

    // Owner account was already selfdestructed
    forAll(stateGen) { stateIn =>
      val (refund, _) = stateIn.stack.pop
      whenever(stateIn.world.getAccount(Address(refund)).isEmpty) {
        val updatedStateIn = stateIn.withAddressToDelete(stateIn.env.ownerAddr)
        val stateOut = op.execute(updatedStateIn)
        verifyGas(G_selfdestruct + G_newaccount, updatedStateIn, stateOut)
        stateOut.gasRefund shouldEqual 0
      }
    }

  }

  verifyAllOpCodesRegistered(except = CREATE, CREATE2, CALL, CALLCODE, DELEGATECALL, STATICCALL, INVALID)
}
