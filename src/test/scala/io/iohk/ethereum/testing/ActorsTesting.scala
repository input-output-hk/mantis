package io.iohk.ethereum.testing
import akka.actor.ActorRef
import akka.testkit.TestActor.AutoPilot

object ActorsTesting {
  def simpleAutoPilot(makeResponse: PartialFunction[Any, Any]): AutoPilot = {
    new AutoPilot {
      def run(sender: ActorRef, msg: Any) = {
        val response = makeResponse.lift(msg)
        response match {
          case Some(value) => sender ! value
          case _           => ()
        }
        this
      }
    }
  }
}
