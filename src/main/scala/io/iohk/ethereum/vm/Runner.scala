package io.iohk.ethereum.vm

import akka.util.ByteString

object Runner {

  def run(hexString: String): Unit = {
    val code = loadCode(hexString)
    println(printCode(code))

//    val result = VM.execute(new Program(code, ByteString.empty))
//    println("Program return:\n" + result.returnData)
//    println("Program storage:\n" + result.storage)
  }

  def show(hexString: String): String =
    printCode(loadCode(hexString))

  private def loadCode(hexString: String): ByteString =
    ByteString(hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  private def printCode(code: ByteString, i: Int = 0): String = {
    if (i >= code.size)
      ""
    else OpCode.byteToOpCode.get(code(i)) match {
      case Some(op: PushOp) =>
        val skip = op.code - PUSH1.code + 1
        val data = code.slice(i + 1, i + skip + 1).map(b => f"$b%02x").mkString(" ")
        s"$op $data\n" + printCode(code, i + skip + 1)

      case Some(op) =>
        s"$op\n" + printCode(code, i + 1)

      case None =>
        f"0x${code(i)}%02x\n" + printCode(code, i + 1)
    }
  }

}
