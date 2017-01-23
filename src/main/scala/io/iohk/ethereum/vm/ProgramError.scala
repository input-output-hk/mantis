package io.iohk.ethereum.vm

sealed trait ProgramError {
  def msg: String
}

case class InvalidOpCode(code: Byte, pc: Int) extends ProgramError {
  val msg: String = f"Invalid opcode: 0x$code%02x at position: $pc"
}

case class InvalidProgramPosition(pc: Int) extends ProgramError {
  val msg: String = s"Invalid program position: $pc"
}

case class DivisionByZero(pc: Int) extends ProgramError {
  val msg: String = s"Division by zero at position: $pc"
}

case class StackOverflow(pc: Int) extends ProgramError {
  val msg: String = s"Stack overflow at position: $pc"
}

case class StackUnderflow(pc: Int) extends ProgramError {
  val msg: String = s"Stack underflow at position: $pc"
}
