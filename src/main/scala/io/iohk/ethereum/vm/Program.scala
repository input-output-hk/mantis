package io.iohk.ethereum.vm

import akka.util.ByteString

import cats.syntax.either._


class Program(code: ByteString) {

  def getByte(pc: Int): Either[ProgramError, Byte] =
    code.lift(pc) match {
      case Some(byte) => byte.asRight
      case None => InvalidCodePosition.asLeft
    }

  def getBytes(from: Int, size: Int): Either[ProgramError, ByteString] = {
    val slice = code.slice(from, from + size)
    if (slice.size == size) slice.asRight else InvalidCodeRange(from, size).asLeft
  }
}
