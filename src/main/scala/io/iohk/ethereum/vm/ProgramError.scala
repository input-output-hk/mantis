package io.iohk.ethereum.vm

sealed trait ProgramError

case class  InvalidOpCode(code: Byte) extends ProgramError
case object InvalidProgramPosition extends ProgramError
case object DivisionByZero extends ProgramError
case object StackOverflow extends ProgramError
case object StackUnderflow extends ProgramError
