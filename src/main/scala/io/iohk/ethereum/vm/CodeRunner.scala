package io.iohk.ethereum.vm

import akka.util.ByteString

object CodeRunner {

  def loadCode(hexString: String): ByteString =
    ByteString(hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  def showBytes(bytes: ByteString): String =
    bytes.map(b => f"$b%02x").mkString

  def printResult(header: String, result: ProgramResult): Unit = {
    val dec = if (result.returnData.length <= 32) s"(${DataWord(result.returnData).intValue})" else ""
    println("\n\n" + header)
    println("-" * header.length)
    println(s"Program return:\n  ${showBytes(result.returnData)} $dec")
    println("Program storage:\n  " + result.storage.underlying.map{case (k, v) => showBytes(k.bytes) + ": " + showBytes(v.bytes)}.mkString("\n  "))
    println("Program error:  \n  " + result.error)
  }

  def main(args: Array[String]): Unit = {

    val filename = "TODO"

    println(s"Compiling $filename")

    // TODO solc --optimize --bin fib2.sol

    val code = "60606040526000600055600060015534610000575b610102806100236000396000f300606060405263ffffffff60e060020a6000350416631e0f97958114602c5780638bae2a0314604b575b6000565b3460005760396004356067565b60408051918252519081900360200190f35b3460005760396082565b60408051918252519081900360200190f35b600080546001556075826089565b600081905590505b919050565b6000545b90565b600060008210156097576000565b81151560a457506000607d565b816001141560b357506001607d565b60bd600283036089565b60c7600184036089565b019050607d565b5b5b5b9190505600a165627a7a723058201d795a86c1971e90f189c5878115e3bafd055d02852ebb2b4882aa89292f695d0029"

    val loadedCode: ByteString = loadCode(code)

    println("Initializing program")

    val program = new Program(loadedCode)
    val invokeCreate = ProgramInvoke(program, ByteString.empty, ByteString.empty, new Storage())

    println("Creating contract")

    val resultCreate = VM.run(invokeCreate)

    printResult("Contract", resultCreate)

    val contract = new Program(resultCreate.returnData)

  }
}
