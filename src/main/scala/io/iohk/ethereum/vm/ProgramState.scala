package io.iohk.ethereum.vm

import akka.util.ByteString

object ProgramState {
  def apply(context: ProgramContext): ProgramState =
    ProgramState(context = context, storage = context.storage)
}

case class ProgramState(
  context: ProgramContext,
  stack: Stack = Stack.empty(),
  memory: Memory = Memory.empty,
  storage: Storage,
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  halted: Boolean = false,
  error: Option[ProgramError] = None
) {

  def program: Program = context.program

  def step(i: Int = 1): ProgramState =
    copy(pc = pc + i)

  def goto(i: Int): ProgramState =
    copy(pc = i)

  def withStack(stack: Stack): ProgramState =
    copy(stack = stack)

  def withMemory(memory: Memory): ProgramState =
    copy(memory = memory)

  def withStorage(storage: Storage): ProgramState =
    copy(storage = storage)

  def withError(error: ProgramError): ProgramState =
    copy(error = Some(error), halted = true)

  def withReturnData(data: ByteString): ProgramState =
    copy(returnData = data)

  def halt: ProgramState =
    copy(halted = true)
}
