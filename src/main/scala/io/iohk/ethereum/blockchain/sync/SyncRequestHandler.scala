package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import akka.actor._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.{Peer, PeerId}
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
    peer.subscribeToDisconnect()
    peer.send(requestMsg)
    peerMessageBus ! Subscribe(subscribeMessageClassifier)
  }

  override def receive: Receive = {
    case MessageFromPeer(responseMsg: ResponseMsg, _) =>
      handleResponseMsg(responseMsg)

    case Timeout =>
      handleTimeout()

    case PeerDisconnected(peerId) if peerId == peer.id =>
      handleTerminated()
  }

  def cleanupAndStop(): Unit = {
    timeout.cancel()
    peer.unsubscribeFromDisconnect()
    peerMessageBus ! Unsubscribe(subscribeMessageClassifier)
    syncController ! Done
    context stop self
  }
}

object SyncRequestHandler {
  case object Done

  private case object Timeout
}
