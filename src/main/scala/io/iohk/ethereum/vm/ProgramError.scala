package io.iohk.ethereum.vm

sealed trait ProgramError
case class  InvalidOpCode(code: Byte) extends ProgramError
case object InvalidCodePosition extends ProgramError

sealed trait StackError extends ProgramError
case object StackOverflow extends StackError
case object StackUnderflow extends StackError
