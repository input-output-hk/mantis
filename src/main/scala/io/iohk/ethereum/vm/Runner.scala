package io.iohk.ethereum.vm

import akka.util.ByteString

// scalastyle:off line.size.limit
object Runner {

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
    if (args.headOption.contains("radek")) {
      // this is mine - bugger off
      main2()
      sys.exit(0)
    }

    println("START")
    val codeStr = "606060405234610000575b60e7806100186000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063a426225414603c575b6000565b3460005760546004808035906020019091905050606a565b6040518082815260200191505060405180910390f35b600060008210156078576000565b60008214156088576000905060b6565b60018214156098576001905060b6565b60a260028303606a565b60ac60018403606a565b01905060b6565b5b5b5b9190505600a165627a7a72305820f45e7f48fa5e8f5ccbe6de0092557b5092052fc1d52be2ca428cf9128e39a6820029"

    val callDataStr = "a42622540000000000000000000000000000000000000000000000000000000000000005"

    val code = loadCode(codeStr)
    val callData = loadCode(callDataStr)

    // println(printCode(code))

    // println(callData)

    val program = new Program(code)

    val result = VM.execute(ProgramInvoke(program, callData, ByteString(), Storage()))

    println(result)



  }

  def main2(): Unit = {
    // solc-js 0.4.8
    val fibonacciHex1 = "6060604052600060005534610000575b61014c8061001e6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680631e0f9795146100495780638bae2a031461007a575b610000565b3461000057610064600480803590602001909190505061009d565b6040518082815260200191505060405180910390f35b34610000576100876100b9565b6040518082815260200191505060405180910390f35b60006100a8826100c4565b60008190555060005490505b919050565b600060005490505b90565b600060008210156100d457610000565b60008214156100e6576000905061011b565b60018214156100f8576001905061011b565b610104600283036100c4565b610110600184036100c4565b01905061011b565b5b5b5b9190505600a165627a7a72305820e2f994be2acb1efe132bce53241fab48101cf527ef7fdcd222a688dfd70650850029"
    // solc 0.4.2
    val fibonacciHex2 = "606060405260006000600050556101408061001a6000396000f360606040526000357c0100000000000000000000000000000000000000000000000000000000900480631e0f9795146100475780638bae2a031461007857610042565b610002565b346100025761006260048080359060200190919050506100a0565b6040518082815260200191505060405180910390f35b346100025761008a60048050506100c6565b6040518082815260200191505060405180910390f35b60006100ab826100d8565b60006000508190555060006000505490506100c1565b919050565b600060006000505490506100d5565b90565b600060008210156100ec576100025661013a565b6000821415610102576000905061013b56610139565b6001821415610118576001905061013b56610138565b610124600283036100d8565b610130600184036100d8565b01905061013b565b5b5b5b91905056"
    // browser-solidity 0.4.8
    val fibonacciHex3 = "6060604052600060005534610000575b61014c8061001e6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680631e0f9795146100495780638bae2a031461007a575b610000565b3461000057610064600480803590602001909190505061009d565b6040518082815260200191505060405180910390f35b34610000576100876100b9565b6040518082815260200191505060405180910390f35b60006100a8826100c4565b60008190555060005490505b919050565b600060005490505b90565b600060008210156100d457610000565b60008214156100e6576000905061011b565b60018214156100f8576001905061011b565b610104600283036100c4565b610110600184036100c4565b01905061011b565b5b5b5b9190505600a165627a7a72305820953725bba7a2662a708556ebcf2e997795d79d649eebce05b4b1f13e4e3ee72d0029"


    def getNewFibHex(i: Int) = "1e0f9795" + f"$i%032x"
    val getStoredFibHex      = "8bae2a03"


    val code = loadCode(fibonacciHex1)
    println(printCode(code))

    val program = new Program(code)
    val callValue = ByteString.empty

    val invoke1 = ProgramInvoke(program, callData = ByteString.empty, callValue, Storage())
    val result1 = VM.execute(invoke1)
    printResult("CONTRACT CREATE", result1)

    val contract = new Program(result1.returnData)

    println("\n\n" + printCode(result1.returnData) + "\n")

    val invoke2 = ProgramInvoke(contract, callData = loadCode(getStoredFibHex), callValue, result1.storage)
    val result2 = VM.execute(invoke2)
    printResult(s"getStoredFib()  [$getStoredFibHex]", result2)

    val fibArg = 5
    val invoke3 = ProgramInvoke(contract, callData = loadCode(getNewFibHex(fibArg)), callValue, result2.storage)
    val result3 = VM.execute(invoke3)
    printResult(s"getNewFib($fibArg)  [${getNewFibHex(fibArg)}", result3)

    val invoke4 = ProgramInvoke(contract, callData = loadCode(getStoredFibHex), callValue, result3.storage)
    val result4 = VM.execute(invoke4)
    printResult(s"getStoredFib()  [$getStoredFibHex]", result4)
  }

  def printResult(header: String, result: ProgramResult): Unit = {
    println("\n\n" + header)
    println("-" * header.length)
    println("Program return:\n\t" + showBytes(result.returnData))
    println("Program storage:\n\t" + result.storage.underlying.map(dw => showBytes(dw.bytes)).mkString("\n\t"))
    println("Program error:  \n\t" + result.error)
  }

  def showBytes(bytes: ByteString): String =
    bytes.map(b => f"$b%02x").mkString
}
