package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import akka.actor._
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.utils.Config.Sync._

class PeerRequestHandler[RequestMsg <: Message, ResponseMsg <: Message : ClassTag]
    (peer: Peer, etcPeerManager: ActorRef, peerEventBus: ActorRef, requestMsg: RequestMsg, responseMsgCode: Int)
    (implicit scheduler: Scheduler, toSerializable: RequestMsg => MessageSerializable)
  extends Actor with ActorLogging {

  import PeerRequestHandler._

  val initiator: ActorRef = context.parent

  val timeout: Cancellable = scheduler.scheduleOnce(peerResponseTimeout, self, Timeout)

  val startTime: Long = System.currentTimeMillis()

  private def subscribeMessageClassifier = MessageClassifier(Set(responseMsgCode), PeerSelector.WithId(peer.id))

  def timeTakenSoFar(): Long = System.currentTimeMillis() - startTime

  override def preStart(): Unit = {
    // TODO: can we just use: peer.ref ! PeerActor.SendMessage() and remove dependency on peer manager? (fix tests)
    etcPeerManager ! EtcPeerManagerActor.SendMessage(toSerializable(requestMsg), peer.id)
    peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
    peerEventBus ! Subscribe(subscribeMessageClassifier)
  }

  override def receive: Receive = {
    case MessageFromPeer(responseMsg: ResponseMsg, _) => handleResponseMsg(responseMsg)
    case Timeout => handleTimeout()
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
    context stop self
  }
}

object PeerRequestHandler {
  def props[RequestMsg <: Message,
            ResponseMsg <: Message : ClassTag]
  (peer: Peer, etcPeerManager: ActorRef, peerEventBus: ActorRef, requestMsg: RequestMsg, responseMsgCode: Int)
  (implicit scheduler: Scheduler, toSerializable: RequestMsg => MessageSerializable): Props =
    Props(new PeerRequestHandler(peer, etcPeerManager, peerEventBus, requestMsg, responseMsgCode))

  case class RequestFailed(peer: Peer, reason: String)
  case class ResponseReceived[T](peer: Peer, response: T, timeTaken: Long)

  private case object Timeout
}
