package io.iohk.ethereum.vm

import akka.util.ByteString

object Assembly {

  sealed trait ByteCode {
    def bytes: ByteString
  }

  implicit class OpCodeAsByteCode(val op: OpCode) extends ByteCode {
    def bytes: ByteString = ByteString(op.code)
  }

  implicit class IntAsByteCode(val i: Int) extends ByteCode {
    def bytes: ByteString = ByteString(i.toByte)
  }

  implicit class ByteAsByteCode(val byte: Byte) extends ByteCode {
    def bytes: ByteString = ByteString(byte)
  }

  implicit class ByteStringAsByteCode(val bytes: ByteString) extends ByteCode
}

import Assembly._

case class Assembly(byteCode: ByteCode*) {
  val code: ByteString = byteCode.foldLeft(ByteString.empty)(_.bytes ++ _.bytes)

  val program: Program = Program(code)

  def linearConstGas(config: EvmConfig): BigInt = byteCode.foldLeft(BigInt(0)) {
    case (g, b: OpCodeAsByteCode) => g + b.op.constGasFn(config.feeSchedule)
    case (g, _)                   => g
  }
}
