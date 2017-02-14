package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

import akka.actor.{Terminated, ActorLogging, Actor, ActorRef}
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.Config.FastSync._

abstract class FastSyncRequestHandler[RequestMsg <: Message : RLPEncoder,
                                      ResponseMsg <: Message : ClassTag](peer: ActorRef)
  extends Actor with ActorLogging {

  import FastSyncRequestHandler._
  import context.system

  def requestMsg: RequestMsg
  def responseMsgCode: Int

  def handleResponseMsg(responseMsg: ResponseMsg): Unit
  def handleTimeout(): Unit

  val fastSyncController = context.parent

  val timeout = system.scheduler.scheduleOnce(peerResponseTimeout, self, Timeout)

  val startTime = System.currentTimeMillis()

  def timeTakenSoFar(): Long = System.currentTimeMillis() - startTime

  override def preStart(): Unit = {
    context watch peer
    peer ! PeerActor.SendMessage(requestMsg)
    peer ! PeerActor.Subscribe(Set(responseMsgCode))
  }

  override def receive: Receive = {
    case PeerActor.MessageReceived(responseMsg: ResponseMsg) =>
      handleResponseMsg(responseMsg)

    case Timeout =>
      handleTimeout()

    case Terminated(`peer`) =>
      cleanupAndStop()
  }

  def cleanupAndStop(): Unit = {
    timeout.cancel()
    context unwatch peer
    peer ! PeerActor.Unsubscribe
    fastSyncController ! Done(peer)
    context stop self
  }
}

object FastSyncRequestHandler {
  case class Done(peer: ActorRef)

  private case object Timeout
}
