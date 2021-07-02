package io.iohk.ethereum.blockchain.sync

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.Props
import akka.actor.Scheduler

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH62._
import io.iohk.ethereum.network.p2p.messages.ETH63.GetNodeData
import io.iohk.ethereum.network.p2p.messages.ETH63.NodeData
import io.iohk.ethereum.utils.Config.SyncConfig

class PeersClient(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val blacklist: Blacklist,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupportNg {
  import PeersClient._

  implicit val ec: ExecutionContext = context.dispatcher

  val statusSchedule: Cancellable =
    scheduler.scheduleWithFixedDelay(syncConfig.printStatusInterval, syncConfig.printStatusInterval, self, PrintStatus)

  def receive: Receive = running(Map())

  override def postStop(): Unit = {
    super.postStop()
    statusSchedule.cancel()
  }

  def running(requesters: Requesters): Receive =
    handlePeerListMessages.orElse {
      case PrintStatus                   => printStatus(requesters: Requesters)
      case BlacklistPeer(peerId, reason) => blacklistIfHandshaked(peerId, syncConfig.blacklistDuration, reason)
      case Request(message, peerSelector, toSerializable) =>
        val requester = sender()
        selectPeer(peerSelector) match {
          case Some(peer) =>
            log.debug("Selected peer {} with address {}", peer.id, peer.remoteAddress.getHostString)
            val handler =
              makeRequest(peer, message, responseMsgCode(message), toSerializable)(scheduler, responseClassTag(message))
            val newRequesters = requesters + (handler -> requester)
            context.become(running(newRequesters))
          case None =>
            log.debug("No suitable peer found to issue a request")
            requester ! NoSuitablePeer
        }
      case PeerRequestHandler.ResponseReceived(peer, message, _) =>
        handleResponse(requesters, Response(peer, message.asInstanceOf[Message]))
      case PeerRequestHandler.RequestFailed(peer, reason) =>
        log.warning(s"Request to peer ${peer.remoteAddress} failed - reason: $reason")
        handleResponse(requesters, RequestFailed(peer, BlacklistReason.RegularSyncRequestFailed(reason)))
    }

  private def makeRequest[RequestMsg <: Message, ResponseMsg <: Message](
      peer: Peer,
      requestMsg: RequestMsg,
      responseMsgCode: Int,
      toSerializable: RequestMsg => MessageSerializable
  )(implicit scheduler: Scheduler, classTag: ClassTag[ResponseMsg]): ActorRef =
    context.actorOf(
      PeerRequestHandler.props[RequestMsg, ResponseMsg](
        peer = peer,
        responseTimeout = syncConfig.peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = requestMsg,
        responseMsgCode = responseMsgCode
      )(classTag, scheduler, toSerializable)
    )

  private def handleResponse[ResponseMsg <: ResponseMessage](requesters: Requesters, responseMsg: ResponseMsg): Unit = {
    val requestHandler = sender()
    requesters.get(requestHandler).foreach(_ ! responseMsg)
    context.become(running(requesters - requestHandler))
  }

  private def selectPeer(peerSelector: PeerSelector): Option[Peer] =
    peerSelector match {
      case BestPeer => bestPeer(peersToDownloadFrom)
    }

  private def responseClassTag[RequestMsg <: Message](requestMsg: RequestMsg): ClassTag[_ <: Message] =
    requestMsg match {
      case _: GetBlockHeaders => implicitly[ClassTag[BlockHeaders]]
      case _: GetBlockBodies  => implicitly[ClassTag[BlockBodies]]
      case _: GetNodeData     => implicitly[ClassTag[NodeData]]
    }

  private def responseMsgCode[RequestMsg <: Message](requestMsg: RequestMsg): Int =
    requestMsg match {
      case _: GetBlockHeaders => Codes.BlockHeadersCode
      case _: GetBlockBodies  => Codes.BlockBodiesCode
      case _: GetNodeData     => Codes.NodeDataCode
    }

  private def printStatus(requesters: Requesters): Unit = {
    log.debug(
      "Request status: requests in progress: {}, available peers: {}",
      requesters.size,
      peersToDownloadFrom.size
    )

    lazy val handshakedPeersStatus = handshakedPeers.map { case (peerId, peerWithInfo) =>
      val peerNetworkStatus = PeerNetworkStatus(
        peerWithInfo.peer,
        isBlacklisted = blacklist.isBlacklisted(peerId)
      )
      (peerNetworkStatus, peerWithInfo.peerInfo)
    }

    log.debug(s"Handshaked peers status (number of peers: ${handshakedPeersStatus.size}): $handshakedPeersStatus")
  }
}

object PeersClient {

  def props(
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(new PeersClient(etcPeerManager, peerEventBus, blacklist, syncConfig, scheduler))

  type Requesters = Map[ActorRef, ActorRef]

  sealed trait PeersClientMessage
  case class BlacklistPeer(peerId: PeerId, reason: BlacklistReason) extends PeersClientMessage
  case class Request[RequestMsg <: Message](
      message: RequestMsg,
      peerSelector: PeerSelector,
      toSerializable: RequestMsg => MessageSerializable
  ) extends PeersClientMessage

  object Request {

    def create[RequestMsg <: Message](message: RequestMsg, peerSelector: PeerSelector)(implicit
        toSerializable: RequestMsg => MessageSerializable
    ): Request[RequestMsg] =
      Request(message, peerSelector, toSerializable)
  }

  case class PeerNetworkStatus(peer: Peer, isBlacklisted: Boolean) {
    override def toString: String =
      s"PeerNetworkStatus {" +
        s" RemotePeerAddress: ${peer.remoteAddress}," +
        s" ConnectionDirection: ${if (peer.incomingConnection) "Incoming" else "Outgoing"}," +
        s" Is blacklisted?: $isBlacklisted" +
        s" }"
  }
  case object PrintStatus extends PeersClientMessage

  sealed trait ResponseMessage
  case object NoSuitablePeer extends ResponseMessage
  case class RequestFailed(peer: Peer, reason: BlacklistReason) extends ResponseMessage
  case class Response[T <: Message](peer: Peer, message: T) extends ResponseMessage

  sealed trait PeerSelector
  case object BestPeer extends PeerSelector

  def bestPeer(peersToDownloadFrom: Map[PeerId, PeerWithInfo]): Option[Peer] = {
    val peersToUse = peersToDownloadFrom.values
      .collect { case PeerWithInfo(peer, PeerInfo(_, chainWeight, true, _, _)) =>
        (peer, chainWeight)
      }

    if (peersToUse.nonEmpty) {
      val (peer, _) = peersToUse.maxBy(_._2)
      Some(peer)
    } else {
      None
    }
  }
}
