package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Scheduler}
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.reflect.ClassTag

class PeersClient(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {
  import PeersClient._

  implicit val ec: ExecutionContext = context.dispatcher

  val statusSchedule: Cancellable = scheduler.schedule(10.seconds, 10.seconds, self, PrintStatus)

  def receive: Receive = running(Map())

  override def postStop(): Unit = {
    super.postStop()
    statusSchedule.cancel()
  }

  def running(requesters: Map[ActorRef, ActorRef]): Receive =
    handleBlacklistMessages orElse handlePeerListMessages orElse {
      case BlacklistPeer(peerId, reason) => peerById(peerId).foreach(blacklistIfHandshaked(_, reason))
      case Request(message, peerSelector, toSerializable) =>
        val requester = sender()
        selectPeer(peerSelector) match {
          case Some(peer) =>
            val handler = makeRequest(peer, message, responseMsgCode(message), toSerializable)
            val newRequesters = requesters + (handler -> requester)
            context become running(newRequesters)
          case None =>
            requester ! NoSuitablePeer
        }
      case ResponseReceived(peer, message, _) =>
        val requestHandler = sender()
        val requester = requesters.get(requestHandler)
        requester.foreach(_ ! Response(peer, message.asInstanceOf[Message]))
        context become running(requesters - requestHandler)
      case PeerRequestHandler.RequestFailed(peer, reason) =>
        val requestHandler = sender()
        val requester = requesters.get(requestHandler)
        requester.foreach(_ ! RequestFailed(peer, reason))
        context become running(requesters - requestHandler)
    }

  private def makeRequest[RequestMsg <: Message, ResponseMsg <: Message](
      peer: Peer,
      requestMsg: RequestMsg,
      responseMsgCode: Int,
      toSerializable: RequestMsg => MessageSerializable)(
      implicit scheduler: Scheduler,
      responseTag: ClassTag[ResponseMsg]): ActorRef = {
    context.actorOf(
      PeerRequestHandler.props[RequestMsg, ResponseMsg](
        peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = requestMsg,
        responseMsgCode = responseMsgCode
      )(responseTag, scheduler, toSerializable))
  }

  private def selectPeer(peerSelector: PeerSelector): Option[Peer] = {
    peerSelector match {
      case BestPeer => bestPeer(peersToDownloadFrom)
    }
  }

  private def responseMsgCode[RequestMsg <: Message](requestMsg: RequestMsg): Int = {
    requestMsg match {
      case _: GetBlockHeaders => BlockHeaders.code
      case _: GetBlockBodies => BlockBodies.code
      case _: GetNodeData => NodeData.code
    }
  }
}
object PeersClient {
  sealed trait PeersClientMessage
  case class BlacklistPeer(peerId: PeerId, reason: String) extends PeersClientMessage
  case class Request[RequestMsg <: Message](
      message: RequestMsg,
      peerSelector: PeerSelector,
      toSerializable: RequestMsg => MessageSerializable)
      extends PeersClientMessage
  object Request {
    def create[RequestMsg <: Message](message: RequestMsg, peerSelector: PeerSelector)(
        implicit toSerializable: RequestMsg => MessageSerializable): Request[RequestMsg] =
      Request(message, peerSelector, toSerializable)
  }
  case object PrintStatus extends PeersClientMessage

  sealed trait ResponseMessage
  case object NoSuitablePeer extends ResponseMessage
  case class RequestFailed(peer: Peer, reason: String) extends ResponseMessage
  case class Response[T <: Message](peer: Peer, message: T) extends ResponseMessage

  sealed trait PeerSelector
  case object BestPeer extends PeerSelector

  def bestPeer(peersToDownloadFrom: Map[Peer, PeerInfo]): Option[Peer] = {
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, PeerInfo(_, totalDifficulty, true, _)) =>
          (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) {
      val (peer, _) = peersToUse.maxBy { case (_, td) => td }
      Some(peer)
    } else {
      None
    }
  }
}
