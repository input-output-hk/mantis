package io.iohk.ethereum.rpcTest.utils

import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.reactive.Observable

import scala.sys.process._

class MantisProcess (
    val underlyingProcess: Process,
    val lines: Observable[String],
    val linesFromBeginning: Observable[String]) {

  def waitForLine(predicate: String => Boolean, predicateToFail: String => Boolean): Task[String] =
    linesFromBeginning
      .mapEval(line =>
        if (predicateToFail(line)) Task.raiseError(new RuntimeException(s"Predicate failed due to line: $line")) else Task.pure(line))
      .filter(predicate)
      .firstL
  def waitForLine(predicate: String => Boolean): Task[String] = waitForLine(predicate, _ => false)

  def waitForRpc(): Task[MantisProcess] = waitForLine(_.contains("JSON RPC HTTP server listening"), _.contains("Cannot start JSON HTTP RPC server"))
    .map(_ => this)

  def waitForDag(): Task[MantisProcess] =
    waitForLine(line => line.contains("DAG") && line.contains("99%"))
      .map(_ => {
        Thread.sleep(5000)
        this
      })

  def nodeAddress(): Task[String] =
    linesFromBeginning
      .tap { line =>
        println(s"line2: $line")
      }
      .map(_.split(' ').lastOption.getOrElse(""))
      .tap(part => {
        println(s"last part: $part")
      })
      .filter(_.startsWith("enode"))
      .firstL
      .tap { address =>
        println(s"address: $address")
      }
}

object MantisProcess extends MantisProcessBuilder {
  def getTempDatadir(): String = MantisProcessBuilder.getTempDatadir()
}
