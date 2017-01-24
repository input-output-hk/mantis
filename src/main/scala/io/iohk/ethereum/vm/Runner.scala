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

  def main(args: Array[String]): Unit = {
    println("START")
    val codeStr = "606060405234610000575b60e7806100186000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063a426225414603c575b6000565b3460005760546004808035906020019091905050606a565b6040518082815260200191505060405180910390f35b600060008210156078576000565b60008214156088576000905060b6565b60018214156098576001905060b6565b60a260028303606a565b60ac60018403606a565b01905060b6565b5b5b5b9190505600a165627a7a72305820f45e7f48fa5e8f5ccbe6de0092557b5092052fc1d52be2ca428cf9128e39a6820029"

    val callDataStr = "a42622540000000000000000000000000000000000000000000000000000000000000005"

    val code = loadCode(codeStr)
    val callData = loadCode(callDataStr)

    // println(printCode(code))

    // println(callData)

    val program = new Program(code, callData)

    val result = VM.execute(ProgramInvoke(program, callData, ByteString()))

    println(result)



  }

}
