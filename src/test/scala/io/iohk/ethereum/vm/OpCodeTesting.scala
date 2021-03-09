package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.MockWorldState.PS
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

trait OpCodeTesting extends AnyFunSuiteLike {
  matchers: Matchers =>

  val config: EvmConfig

  lazy val unaryOps: List[UnaryOp] = config.opCodes.collect { case op: UnaryOp => op }
  lazy val binaryOps: List[BinaryOp] = config.opCodes.collect { case op: BinaryOp => op }
  lazy val ternaryOps: List[TernaryOp] = config.opCodes.collect { case op: TernaryOp => op }
  lazy val constOps: List[ConstOp] = config.opCodes.collect { case op: ConstOp => op }
  lazy val pushOps: List[PushOp] = config.opCodes.collect { case op: PushOp => op }
  lazy val dupOps: List[DupOp] = config.opCodes.collect { case op: DupOp => op }
  lazy val swapOps: List[SwapOp] = config.opCodes.collect { case op: SwapOp => op }
  lazy val logOps: List[LogOp] = config.opCodes.collect { case op: LogOp => op }
  lazy val constGasOps: List[OpCode with ConstGas] = config.opCodes.collect { case op: ConstGas if op != INVALID => op }

  def test[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => test(op.toString)(f(op)))

  def ignore[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => ignore(op.toString)(f(op)))

  /** Run this as the last test in the suite
    * Ignoring an OpCode test will NOT cause this test to fail
    */
  def verifyAllOpCodesRegistered(except: OpCode*): Unit =
    test("all opcodes have been registered") {
      val untested = config.opCodes.filterNot(op => testNames(op.toString)).diff(except)
      if (untested.isEmpty)
        succeed
      else
        fail("Unregistered opcodes: " + untested.mkString(", "))
    }

  def verifyGas(expectedGas: BigInt, stateIn: PS, stateOut: PS, allowOOG: Boolean = true): Unit =
    if (stateOut.error.contains(OutOfGas) && allowOOG)
      stateIn.gas should be < expectedGas
    else if (stateOut.error.contains(OutOfGas) && !allowOOG)
      fail(s"Unexpected $OutOfGas error")
    else if (
      stateOut.error.isDefined && stateOut.error.collect {
        case InvalidJump(_)     => ()
        case RevertOccurs       => ()
        case ReturnDataOverflow => ()
      }.isEmpty
    ) {
      //Found error that is neither an InvalidJump nor RevertOccurs
      fail(s"Unexpected ${stateOut.error.get} error")
    } else {
      stateOut.gas shouldEqual (stateIn.gas - expectedGas)
    }
}
