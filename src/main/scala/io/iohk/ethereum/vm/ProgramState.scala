package io.iohk.ethereum.vm

case class ProgramState(
  stack: Stack = Stack(),
  memory: Memory = Memory(),
  storage: Storage = new Storage,
  pc: Int = 0,
  halt: Boolean = false
)
