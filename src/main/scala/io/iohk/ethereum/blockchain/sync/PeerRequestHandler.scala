package io.iohk.ethereum.blockchain.sync

import akka.actor._
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerDisconnectedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.Unsubscribe
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

class PeerRequestHandler[RequestMsg <: Message, ResponseMsg <: Message: ClassTag](
    peer: Peer,
    responseTimeout: FiniteDuration,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    requestMsg: RequestMsg,
    responseMsgCode: Int
)(implicit scheduler: Scheduler, toSerializable: RequestMsg => MessageSerializable)
    extends Actor
    with ActorLogging {

  import PeerRequestHandler._

  val initiator: ActorRef = context.parent

  val timeout: Cancellable = scheduler.scheduleOnce(responseTimeout, self, Timeout)

  val startTime: Long = System.currentTimeMillis()

  private def subscribeMessageClassifier = MessageClassifier(Set(responseMsgCode), PeerSelector.WithId(peer.id))

  def timeTakenSoFar(): Long = System.currentTimeMillis() - startTime

  override def preStart(): Unit = {
    etcPeerManager ! EtcPeerManagerActor.SendMessage(toSerializable(requestMsg), peer.id)
    peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
    peerEventBus ! Subscribe(subscribeMessageClassifier)
  }

  override def receive: Receive = {
    case MessageFromPeer(responseMsg: ResponseMsg, _)  => handleResponseMsg(responseMsg)
    case Timeout                                       => handleTimeout()
    case PeerDisconnected(peerId) if peerId == peer.id => handleTerminated()
  }

  def handleResponseMsg(responseMsg: ResponseMsg): Unit = {
    cleanupAndStop()
    initiator ! ResponseReceived(peer, responseMsg, timeTaken = timeTakenSoFar())
  }

  def handleTimeout(): Unit = {
    cleanupAndStop()
    initiator ! RequestFailed(peer, "request timeout")
  }

  def handleTerminated(): Unit = {
    cleanupAndStop()
    initiator ! RequestFailed(peer, "connection closed")
  }

  def cleanupAndStop(): Unit = {
    timeout.cancel()
    peerEventBus ! Unsubscribe()
    context.stop(self)
  }
}

object PeerRequestHandler {
  def props[RequestMsg <: Message, ResponseMsg <: Message: ClassTag](
      peer: Peer,
      responseTimeout: FiniteDuration,
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      requestMsg: RequestMsg,
      responseMsgCode: Int
  )(implicit scheduler: Scheduler, toSerializable: RequestMsg => MessageSerializable): Props =
    Props(new PeerRequestHandler(peer, responseTimeout, etcPeerManager, peerEventBus, requestMsg, responseMsgCode))

  case class RequestFailed(peer: Peer, reason: String)
  case class ResponseReceived[T](peer: Peer, response: T, timeTaken: Long)

  private case object Timeout
}
