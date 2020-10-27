package io.iohk.ethereum.forking

import cats.effect.ExitCode
import cats.implicits._
import io.iohk.ethereum.forking.ForkChecker.BlockCheck
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.concurrent.duration.DurationInt

object ForkApp extends TaskApp {
  def run(args: List[String]): Task[ExitCode] =
    Env()
      .use(env =>
        Observable
          .repeatEvalF(for {
            blockChecks <- env.forkChecker.findFork
            distribution <- env.forkChecker.checkMiningDistribution
            _ <- Console.doPrint(blockChecks)(BlockCheck.multipleBlockChecksShow)
            _ <- Console.doPrint(distribution)
            _ <- Console.doPrint("").delayResult(10.seconds)
          } yield ())
          .takeUntil(Observable.fromFuture(env.actorSystem.whenTerminated))
          .lastL
      )
      .as(ExitCode.Success)
}
