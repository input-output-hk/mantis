package io.iohk.ethereum.vm

import akka.util.ByteString

/**
  * Holds a program's code and provides utilities for accessing it (defaulting to zeroes when out of scope)
  *
  * @param code the EVM bytecode as bytes
  */
case class Program(code: ByteString) {

  def getByte(pc: Int): Byte =
    code.lift(pc).getOrElse(0)

  def getBytes(from: Int, size: Int): ByteString =
    code.slice(from, from + size).padTo(size, 0.toByte)
}
