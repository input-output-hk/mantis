package io.iohk.ethereum.vm

import akka.util.ByteString

import cats.syntax.either._


case class Program(code: ByteString) {

  def getByte(pc: Int): Either[ProgramError, Byte] =
    code.lift(pc) match {
      case Some(byte) => byte.asRight
      // TODO perhaps we should simply return 0 (STOP)
      case None => InvalidCodePosition.asLeft
    }

  def getBytes(from: Int, size: Int): ByteString =
    code.slice(from, from + size).padTo(size, 0)
}
