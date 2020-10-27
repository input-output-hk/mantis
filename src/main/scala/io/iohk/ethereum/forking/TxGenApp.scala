package io.iohk.ethereum.forking

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable
import cats.implicits._
import cats._
import ShowInstances._
import akka.util.ByteString

import scala.concurrent.duration.DurationInt
import scala.util.Random

object TxGenApp extends TaskApp {
  implicit val byteStringEq: Eq[ByteString] = (a: ByteString, b: ByteString) => a.equals(b)

  def run(args: List[String]): Task[ExitCode] = {
    Env().use { env =>
      val transactions$ = Observable
        .fromTask(env.txGenerators)
        .mergeMap(generators => Observable.from(generators))
        .mergeMap(_.sentTransactions)
        .map(txHash => show"Sent tx: ${txHash}")

      val blocks$ = Observable
        .repeatEvalF {
          for {
            client <- Task { Random.shuffle(env.ethRpcClients).head }
            lastBlock <- client.lastBlock.peel.delayExecution(1.second)
          } yield lastBlock
        }
        .collect { case Some(block) => block }
        .distinctUntilChangedByKey(block => (block.number, block.hash))
        .map(block => show"New block: ${block.number}, ${block.hash}, ${block.transactions.size}")

      Observable(transactions$, blocks$).merge
        .mapEval(entry => Console.doPrint(entry))
        .takeUntil(Observable.fromFuture(env.actorSystem.whenTerminated))
        .countL
        .as(ExitCode.Success)
    }
  }
}
