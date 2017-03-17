package io.iohk.ethereum.vm

import org.scalatest.FunSuiteLike


trait OpCodeTesting extends FunSuiteLike {

  val unaryOps = OpCode.opcodes.collect { case op: UnaryOp => op }
  val binaryOps = OpCode.opcodes.collect { case op: BinaryOp => op }
  val ternaryOps = OpCode.opcodes.collect { case op: TernaryOp => op }
  val constOps = OpCode.opcodes.collect { case op: ConstOp => op }
  val pushOps = OpCode.opcodes.collect { case op: PushOp => op }
  val dupOps = OpCode.opcodes.collect { case op: DupOp => op }
  val swapOps = OpCode.opcodes.collect { case op: SwapOp => op }
  val logOps = OpCode.opcodes.collect { case op: LogOp => op }
  val constGasOps = OpCode.opcodes.collect { case op: ConstGas if op != INVALID => op }

  def test[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => test(op.toString)(f(op)))

  def ignore[T <: OpCode](ops: T*)(f: T => Any): Unit =
    ops.foreach(op => ignore(op.toString)(f(op)))

  /**
    * Run this as the last test in the suite
    * Ignoring an OpCode test will NOT cause this test to fail
    */
  def verifyAllOpCodesRegistered(except: OpCode*): Unit = {
    test("all opcodes have been registered") {
      val untested = OpCode.opcodes.filterNot(op => testNames(op.toString)).diff(except)
      if (untested.isEmpty)
        succeed
      else
        fail("Unregistered opcodes: " + untested.mkString(", "))
    }
  }
}
