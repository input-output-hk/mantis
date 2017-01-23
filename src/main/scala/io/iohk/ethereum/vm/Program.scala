package io.iohk.ethereum.vm

import akka.util.ByteString


class Program(code: ByteString, callData: ByteString) {

  def getByte(pc: Int): Either[ProgramError, Byte] =
    code.lift(pc) match {
      case Some(byte) => Right(byte)
      case None => Left(InvalidProgramPosition(pc))
    }

  def getBytes(pc: Int, n: Int): ByteString = {
    // An exception should never be thrown as long the function is applied by valid opcodes
    require(n > 0 && n <= 32, "Invalid number of bytes to retrieve from code")

    code.slice(pc, pc + n)
  }

  def getCallData(offset: Int): ByteString = {
    require(offset < callData.length, "Invalid call data offset")
    callData.slice(offset, offset + 32)
  }
}
