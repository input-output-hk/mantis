package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import akka.actor._
import io.iohk.ethereum.network.PeerMessageBusActor._
import io.iohk.ethereum.network.{Peer, PeerActor}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.Config.FastSync._

abstract class SyncRequestHandler[RequestMsg <: Message : RLPEncoder,
                                  ResponseMsg <: Message : ClassTag](peer: Peer, peerMessageBus: ActorRef)
                                                                    (implicit scheduler: Scheduler)
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

  private def subscribeMessageClassifier = MessageClassifier(Set(responseMsgCode), PeerSelector.WithId(peer.id))

  def timeTakenSoFar(): Long = System.currentTimeMillis() - startTime

  override def preStart(): Unit = {
    context watch peer.ref
    peer.ref ! PeerActor.SendMessage(requestMsg)
    peerMessageBus ! Subscribe(subscribeMessageClassifier)
  }

  override def receive: Receive = {
    case MessageFromPeer(responseMsg: ResponseMsg, _) =>
      handleResponseMsg(responseMsg)

    case Timeout =>
      handleTimeout()

    case Terminated(ref) if ref == peer.ref =>
      handleTerminated()
  }

  def cleanupAndStop(): Unit = {
    timeout.cancel()
    context unwatch peer.ref
    peerMessageBus ! Unsubscribe(subscribeMessageClassifier)
    syncController ! Done
    context stop self
  }
}

object SyncRequestHandler {
  case object Done

  private case object Timeout
}
