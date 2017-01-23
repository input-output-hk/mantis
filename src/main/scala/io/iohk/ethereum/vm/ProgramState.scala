package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramState(
  program: Program,
  stack: Stack = Stack.Empty,
  memory: Memory = Memory(),
  storage: Storage = Storage(),
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  halt: Boolean = false,
  error: Option[ProgramError] = None
) {

  def verify: ProgramState = {
    if (stack.underflown)
      copy(error = Some(StackUnderflow(pc)), halt = true)
    else if (stack.overflown)
      copy(error = Some(StackOverflow(pc)), halt = true)
    else
      this
  }

  def step(i: Int = 1): ProgramState =
    copy(pc = pc + i)

  def goto(i: Int): ProgramState =
    copy(pc = i)
}

