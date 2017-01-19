package io.iohk.ethereum.vm


class Program(code: IndexedSeq[Byte]) {

  def getOpCode(pc: Int): Option[OpCode] =
    code.lift(pc).map(OpCode.byteToOpCode)
}
