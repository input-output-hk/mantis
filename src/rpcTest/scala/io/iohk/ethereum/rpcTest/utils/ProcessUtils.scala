package io.iohk.ethereum.rpcTest.utils

import cats.effect.Resource
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.duration._
import scala.sys.process.{Process, ProcessLogger}

object ProcessUtils {
  def kill(process: Process): Task[Unit] = {
    Task { process.destroy() }
        .flatMap(_ => Observable.interval(10.millis).filter(_ => !process.isAlive()).firstL)
        .map(_ => ())
  }

  def run(cmd: String): Resource[Task, Process] =
    Resource.make(
      Task {
        val logger = ProcessLogger(line => println(s"miner: $line"))

        Process(cmd).run(logger)
      }
    )(kill)
}
