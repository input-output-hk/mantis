package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
/*import rapture._
import core._
import io._
import net._
import http._
import uri._
import json._
import codec._
import encodings.system._
import jsonBackends.jackson._
import jsonInterop._
import formatters.humanReadable._
import sys.process._*/

// scalastyle:off magic.number
object CodeRunner {
/*
  def loadCode(hexString: String): ByteString =
    ByteString(hexString.trim.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  def showBytes(bytes: ByteString): String =
    bytes.map(b => f"$b%02x").mkString

  def printResult(header: String, verbose: Boolean, result: ProgramResult, time: Long): Unit = {
    val dec = if (result.returnData.length <= 32) s"(${DataWord(result.returnData).intValue})" else ""
    println("\n\n" + header)
    println("-" * header.length)
    if (verbose) {
      println(s"Program return:\n  ${showBytes(result.returnData)} $dec")
      println("Program storage:\n  " + result.storage.toMap.map{case (k, v) => showBytes(k.bytes) + ": " + showBytes(v.bytes)}.mkString("\n  "))
    } else {
      println(s"Program return:\n $dec")
    }
    if (result.error.isEmpty) {
      println("Gas spent: " + result.gasUsed)
    }
    result.error foreach { e => println("Program error:  \n  " + e) }
    println(s"Time taken: $time ms")
  }

  def functionSignatureHash(functionSignature: String): String =
    sha3(functionSignature.getBytes).take(4).map(b => f"$b%02x").mkString

  def encodeUint256(n: BigInt): String = {
    showBytes(DataWord(n).bytes)
  }

  def getAbi(filename: String): Json = {
    s"solcjs --abi $filename".!
    val abi = s"cat ${filename.capitalize.replaceAll("sol$", "abi")}".!!
    Json.parse(abi)
  }

  def getBytecode(filename: String): String = {
    s"solcjs --bin --optimize $filename".!
    s"cat ${filename.capitalize.replaceAll("sol$", "bin")}".!!
  }

  def printFunctions(filename: String): Unit = {
    println("Available functions:")
    getAbi(filename).as[Seq[Json]].foreach {json =>
      val name = json.name.as[String]
      val inputs = json.inputs.as[Seq[Json]].map(o => o.`type`.as[String]).mkString(",")
      val outputs = json.outputs.as[Seq[Json]].map(o => o.`type`.as[String]).mkString(",")
      println(s"$name($inputs): $outputs")
    }
  }

  // supports only uint256
  def commandToHashAndGas(cmd: String): (String, BigInt) = {
    val call :: tail = cmd.split("/").toList
    val functionSignature = call.replaceAll("[0-9]+", "uint256")
    val params = call.split(",")
      .map(_.replaceAll("[^0-9]+", ""))
      .filterNot(_.isEmpty)
      .map(BigInt(_))
    val hash = functionSignatureHash(functionSignature) + params.map(encodeUint256(_)).mkString
    val gas = tail.headOption.map(BigInt(_)).getOrElse(DataWord.MaxValue.toBigInt)
    (hash, gas)
  }

  def compileAndRun(filename: String, verbose: Boolean, commands: Seq[String]): Unit = {

    val code = getBytecode(filename)
    val loadedCode: ByteString = loadCode(code)

    println("Initializing program...")

    val program = Program(loadedCode)
    val invokeCreate = ProgramContext(program, gas = DataWord.MaxValue, ByteString.empty, ByteString.empty, Storage.fromSeq(Seq()))

    println("Creating contract...")

    val resultCreate = VM.run(invokeCreate)
    val contract = Program(resultCreate.returnData)

    println("Running function(s)...")

    val command = commands.head

    commands.foldLeft(resultCreate){(previousResult, command) =>
      val (paramsHex, gas) = commandToHashAndGas(command)
      val invokeNew = ProgramContext(contract, gas, callData = loadCode(paramsHex), ByteString.empty, previousResult.storage)
      val t0 = System.currentTimeMillis()
      val resultNew = VM.run(invokeNew)
      printResult(s"$command", verbose, resultNew, System.currentTimeMillis() - t0)
      resultNew
    }

  }

  def main(args: Array[String]): Unit = {

    args.toList match {
      case "--list-functions" :: filename :: Nil => printFunctions(filename)
      case "--run" :: filename :: commands => compileAndRun(filename, false, commands)
      case "--run-verbose" :: filename :: commands => compileAndRun(filename, true, commands)
      case _ =>
    }

  }
  */
}
