package io.iohk.ethereum.vm

import akka.util.ByteString

import cats.syntax.either._


class Program(code: ByteString, callData: ByteString) {

  def getByte(pc: Int): Either[ProgramError, Byte] =
    code.lift(pc) match {
      case Some(byte) => byte.asRight
      case None => InvalidProgramPosition.asLeft
    }

  def getBytes(pc: Int, n: Int): ByteString = {
    // An exception should never be thrown as long the function is applied by valid opcodes
    require(n > 0 && n <= 32, "Invalid number of bytes to retrieve from code")

    code.slice(pc, pc + n)
  }
}
