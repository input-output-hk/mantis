package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramState(
  program: Program,
  stack: Stack = Stack(),
  memory: Memory = Memory(),
  storage: Storage = Storage(),
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  halt: Boolean = false,
  error: Option[ProgramError] = None
) {

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
    copy(error = Some(error), halt = true)
}

