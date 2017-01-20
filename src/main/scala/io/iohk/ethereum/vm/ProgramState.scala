package io.iohk.ethereum.vm

case class ProgramState(
  program: Program,
  stack: Stack = Stack(),
  memory: Memory = Memory(),
  storage: Storage = new Storage,
  pc: Int = 0,
  halt: Boolean = false,
  error: Option[ProgramError] = None
)
