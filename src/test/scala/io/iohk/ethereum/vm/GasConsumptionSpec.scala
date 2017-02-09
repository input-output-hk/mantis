package io.iohk.ethereum.vm

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import Generators._
import GasFee._

class GasConsumptionSpec extends FunSuite with Matchers with PropertyChecks {

  val constGasOps = OpCode.opcodes.collect { case op: ConstGas => op }
  val pushOps = OpCode.opcodes.collect { case op: PushOp => op }
  val dupOps = OpCode.opcodes.collect { case op: DupOp => op }
  val swapOps = OpCode.opcodes.collect { case op: SwapOp => op }

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


  def test[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => test(op.toString)(f(op)))


  test(constGasOps: _*) { op =>
    val maxStackElemsForOp = 17 // DUP* and SWAP*
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = maxStackElemsForOp),
      gasGen = getBigIntGen(max = op.constGas.value * 2))

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      if (stateIn.gas >= op.constGas.value)
        stateOut.gas shouldEqual (stateIn.gas - constGasFees(op))
      else
        stateOut shouldEqual stateIn.withError(OutOfGas)
    }
  }

  test(MSTORE) { op =>
    val maxGasUsage = 50000 //FIXME: twice actual max
    val memSize = 256
    val stateGen = getProgramStateGen(
      stackGen = getStackGen(elems = 2, maxWord = DataWord(memSize / 2)),
      gasGen = getBigIntGen(max = maxGasUsage),
      memGen = getMemoryGen(memSize)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)
      val (addr, _) = stateIn.stack.pop
      val expectedGas = G_verylow.value + calcMemCost(stateIn.memory.size, addr, DataWord.Size) //TODO: test calcMemCost separately

      if (expectedGas >= stateIn.gas)
        stateOut.gas shouldEqual (stateIn.gas - expectedGas)
      else
        stateOut shouldEqual stateIn.withError(OutOfGas)
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
      val oldValue = stateOut.storage.load(addr)
      val expectedGas = if (oldValue.isZero && !value.isZero) G_sset.value else G_sreset.value
      val expectedRefund = if (value.isZero && !oldValue.isZero) R_sclear.value else BigInt(0)

      if (stateIn.gas >= expectedGas) {
        stateOut.gas shouldEqual (stateIn.gas - expectedGas)
        stateOut.gasRefund shouldEqual (stateIn.gasRefund + expectedRefund)
      } else {
        stateOut shouldEqual stateIn.withError(OutOfGas)
      }
    }
  }

}
