package io.iohk.ethereum.vm

/**
  * Marker trait for errors that may occur during program execution
  */
sealed trait ProgramError
case class  InvalidOpCode(code: Byte) extends ProgramError

sealed trait StackError extends ProgramError
case object StackOverflow extends StackError
case object StackUnderflow extends StackError
