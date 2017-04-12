package io.iohk.ethereum.vm

import akka.util.ByteString

object Assembly {

  sealed trait ByteCode {
    def byte: Byte
  }

  implicit class OpCodeAsByteCode(val op: OpCode) extends ByteCode {
    def byte: Byte = op.code
  }

  implicit class IntAsByteCode(val i: Int) extends ByteCode {
    def byte: Byte = i.toByte
  }
}

import Assembly._

case class Assembly(byteCode: ByteCode*) {
  val code: ByteString = ByteString(byteCode.map(_.byte): _*)

  val program: Program = Program(code)
}
