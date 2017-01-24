package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramInvoke(program: Program, callData: ByteString, callValue: ByteString, storage: Storage) {
  require(callValue.length <= 32, "Invalid callValue")

  def getCallData(offset: Int): ByteString = {
    require(offset < callData.length, "Invalid call data offset")
    callData.slice(offset, offset + 32)
  }
}
