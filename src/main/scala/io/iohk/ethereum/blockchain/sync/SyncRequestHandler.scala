package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import akka.actor._
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.utils.Config.FastSync._

abstract class SyncRequestHandler[RequestMsg <: Message,
                                  ResponseMsg <: Message : ClassTag](peer: ActorRef)
                                  (implicit scheduler: Scheduler, toSerializable: RequestMsg => MessageSerializable)
  extends Actor with ActorLogging {

  import SyncRequestHandler._

  def requestMsg: RequestMsg
  def responseMsgCode: Int

  def handleResponseMsg(responseMsg: ResponseMsg): Unit
  def handleTimeout(): Unit
  def handleTerminated(): Unit

  val syncController: ActorRef = context.parent

  val timeout: Cancellable = scheduler.scheduleOnce(peerResponseTimeout, self, Timeout)

  val startTime: Long = System.currentTimeMillis()

  def timeTakenSoFar(): Long = System.currentTimeMillis() - startTime

  override def preStart(): Unit = {
    context watch peer
    peer ! PeerActor.SendMessage(toSerializable(requestMsg))
    peer ! PeerActor.Subscribe(Set(responseMsgCode))
  }

  override def receive: Receive = {
    case PeerActor.MessageReceived(responseMsg: ResponseMsg) =>
      handleResponseMsg(responseMsg)

    case Timeout =>
      handleTimeout()

    case Terminated(`peer`) =>
      handleTerminated()
  }

  def cleanupAndStop(): Unit = {
    timeout.cancel()
    context unwatch peer
    peer ! PeerActor.Unsubscribe
    syncController ! Done
    context stop self
  }
}

object SyncRequestHandler {
  case object Done

  private case object Timeout
}
