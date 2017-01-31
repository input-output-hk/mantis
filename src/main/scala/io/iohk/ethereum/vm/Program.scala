package io.iohk.ethereum.vm

import akka.util.ByteString

case class Program(code: ByteString) {

  def getByte(pc: Int): Byte =
    code.lift(pc).getOrElse(0)

  def getBytes(from: Int, size: Int): ByteString =
    code.slice(from, from + size).padTo(size, 0.toByte)
}
