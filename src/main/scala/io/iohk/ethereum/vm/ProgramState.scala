package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramState(
  program: Program,
  stack: Stack = Stack(),
  memory: Memory = Memory(),
  storage: Storage = new Storage,
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  halt: Boolean = false,
  error: Option[ProgramError] = None
)
