package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramInvoke(program: Program, callData: ByteString, callValue: ByteString, storage: Storage) {
  require(callValue.length <= 32, "Invalid callValue")
}
