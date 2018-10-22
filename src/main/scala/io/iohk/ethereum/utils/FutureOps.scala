package io.iohk.ethereum.utils
import akka.actor.Scheduler

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration

object FutureOps {
  implicit class FutureOps[T](future: Future[T]) {

    def delayedBy(delay: FiniteDuration)(implicit ec: ExecutionContext, scheduler: Scheduler): Future[T] =
      future.flatMap(value => {
        val p = Promise[T]()
        scheduler.scheduleOnce(delay, new Runnable { def run(): Unit = p.success(value) })
        p.future
      })
  }
}
