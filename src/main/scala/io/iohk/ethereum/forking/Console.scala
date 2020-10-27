package io.iohk.ethereum.forking
import cats.Show
import cats.implicits._
import monix.eval.Task

object Console {
  def doPrint[T](thing: T)(implicit thingShow: Show[T]): Task[Unit] =
    Task { println(thing.show) }
}
