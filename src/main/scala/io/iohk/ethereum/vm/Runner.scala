package io.iohk.ethereum.vm

import akka.util.ByteString

// scalastyle:off line.size.limit
/**
  * This object is for experiments only. It will be removed ultimately.
  */
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
    // solc-js 0.4.8
    val fibonacciHex1 = "6060604052600060005534610000575b61014c8061001e6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680631e0f9795146100495780638bae2a031461007a575b610000565b3461000057610064600480803590602001909190505061009d565b6040518082815260200191505060405180910390f35b34610000576100876100b9565b6040518082815260200191505060405180910390f35b60006100a8826100c4565b60008190555060005490505b919050565b600060005490505b90565b600060008210156100d457610000565b60008214156100e6576000905061011b565b60018214156100f8576001905061011b565b610104600283036100c4565b610110600184036100c4565b01905061011b565b5b5b5b9190505600a165627a7a72305820e2f994be2acb1efe132bce53241fab48101cf527ef7fdcd222a688dfd70650850029"
    // solc 0.4.2
    val fibonacciHex2 = "606060405260006000600050556101408061001a6000396000f360606040526000357c0100000000000000000000000000000000000000000000000000000000900480631e0f9795146100475780638bae2a031461007857610042565b610002565b346100025761006260048080359060200190919050506100a0565b6040518082815260200191505060405180910390f35b346100025761008a60048050506100c6565b6040518082815260200191505060405180910390f35b60006100ab826100d8565b60006000508190555060006000505490506100c1565b919050565b600060006000505490506100d5565b90565b600060008210156100ec576100025661013a565b6000821415610102576000905061013b56610139565b6001821415610118576001905061013b56610138565b610124600283036100d8565b610130600184036100d8565b01905061013b565b5b5b5b91905056"
    // browser-solidity 0.4.8
    val fibonacciHex3 = "6060604052600060005534610000575b61014c8061001e6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680631e0f9795146100495780638bae2a031461007a575b610000565b3461000057610064600480803590602001909190505061009d565b6040518082815260200191505060405180910390f35b34610000576100876100b9565b6040518082815260200191505060405180910390f35b60006100a8826100c4565b60008190555060005490505b919050565b600060005490505b90565b600060008210156100d457610000565b60008214156100e6576000905061011b565b60018214156100f8576001905061011b565b610104600283036100c4565b610110600184036100c4565b01905061011b565b5b5b5b9190505600a165627a7a72305820953725bba7a2662a708556ebcf2e997795d79d649eebce05b4b1f13e4e3ee72d0029"
    // browser-solidity 0.4.8 optimised + prevFib
    val fibonacciHex4 = "60606040526000600055600060015534610000575b610102806100236000396000f300606060405263ffffffff60e060020a6000350416631e0f97958114602c5780638bae2a0314604b575b6000565b3460005760396004356067565b60408051918252519081900360200190f35b3460005760396082565b60408051918252519081900360200190f35b600080546001556075826089565b600081905590505b919050565b6000545b90565b600060008210156097576000565b81151560a457506000607d565b816001141560b357506001607d565b60bd600283036089565b60c7600184036089565b019050607d565b5b5b5b9190505600a165627a7a7230582033531a7f05bbeab4499ea1904099566383a9dd25f524242b9b855024d3b458d30029"

    def getNewFibHex(i: Int) = "1e0f9795" + f"$i%064x"
    val getStoredFibHex      = "8bae2a03"


    val code = loadCode(fibonacciHex4)
    println(printCode(code))

    val program = Program(code)
    val callValue = ByteString.empty

    val contextCreate = ProgramContext(program, 0, callData = ByteString.empty, callValue, Storage.Empty)
    val resultCreate = VM.run(contextCreate)
    printResult("CONTRACT CREATE", resultCreate)

    val contract = Program(resultCreate.returnData)

    val resultAfterIter = (1 to 10).foldLeft(resultCreate){ (previousResult, i) =>
      val contextStored = ProgramContext(contract, 0, callData = loadCode(getStoredFibHex), callValue, previousResult.storage)
      val resultStored = VM.run(contextStored)
      printResult(s"getStoredFib()  [$getStoredFibHex]", resultStored)

      val contextNew = ProgramContext(contract, DataWord.MaxWord.toBigInt, callData = loadCode(getNewFibHex(i)), callValue, resultStored.storage)
      val resultNew = VM.run(contextNew)
      printResult(s"getNewFib($i)  [${getNewFibHex(i)}]", resultNew)

      resultNew
    }

    val contextFinal = ProgramContext(contract, DataWord.MaxWord.toBigInt, callData = loadCode(getStoredFibHex), callValue, resultAfterIter.storage)
    val resultFinal = VM.run(contextFinal)
    printResult(s"getStoredFib()  [$getStoredFibHex]", resultFinal)
  }

  def printResult(header: String, result: ProgramResult): Unit = {
    val dec = if (result.returnData.length <= 32) s"(${DataWord(result.returnData).intValue})" else ""
    println("\n\n" + header)
    println("-" * header.length)
    println(s"Program return:\n  ${showBytes(result.returnData)} $dec")
    println("Program storage:\n  " + result.storage.toMap.toList.sortBy(_._1).map(kv => showBytes(kv._2.bytes)).mkString("\n  "))
    println("Program error:  \n  " + result.error)
  }

  def showBytes(bytes: ByteString): String =
    bytes.map(b => f"$b%02x").mkString
}
