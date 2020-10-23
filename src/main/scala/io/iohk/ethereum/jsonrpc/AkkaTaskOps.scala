package io.iohk.ethereum.jsonrpc

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import monix.eval.Task

object AkkaTaskOps { self: ActorRef =>
  implicit class TaskActorOps(to: ActorRef) {
    import akka.pattern.ask

    def askFor[A](message: Any)(implicit timeout: Timeout, sender: ActorRef = Actor.noSender): Task[A] =
      Task.fromFuture(to ? message).timeout(timeout.duration).map(_.asInstanceOf[A])
  }
}
