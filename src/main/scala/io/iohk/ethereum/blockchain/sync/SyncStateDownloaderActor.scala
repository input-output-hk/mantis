package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler, Timers}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor._
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{GetMissingNodes, MissingNodes}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.annotation.tailrec
import scala.concurrent.duration._

/**
  * SyncStateDownloaderActor receives missing nodes from scheduler and makes sure that those nodes would be eventually retrieved.
  * It never ask ask two peers for the same nodes.
  * Another design choice would be to allow for duplicate node retrieval and ignore duplicates at scheduler level, but to do that
  * SyncStateDownloaderActor would need to keep track which peer was already asked for which node.
  */
class SyncStateDownloaderActor(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with BlacklistSupport
    with PeerListSupport
    with Timers {

  private def requestNodes(request: PeerRequest): ActorRef = {
    log.info(s"Requesting ${request.nodes.size} from peer ${request.peer}")
    val handler = context.actorOf(
      PeerRequestHandler.props[GetNodeData, NodeData](
        request.peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetNodeData(request.nodes),
        responseMsgCode = NodeData.code
      )
    )
    context.watchWith(handler, RequestTerminated(request.peer))
  }

  private def getFreePeers(state: DownloaderState) = {
    handshakedPeers.collect {
      case (peer, _) if !state.activeRequests.contains(peer.id) && !isBlacklisted(peer.id) => peer
    }
  }

  def handleRequestResults(scheduler: ActorRef, currentState: DownloaderState): Receive = {
    case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
      log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
      context unwatch (sender())
      currentState.handleRequestSuccess(peer, nodeData) match {
        case (UnrequestedResponse, newState) =>
          log.debug("Received unexpected response from {}", peer.id)
          // just ignore unrequested stuff
          context.become(downloading(scheduler, newState))
        case (NoUsefulDataInResponse, newState) =>
          log.debug("Received no useful data from peer {}, blacklisting", {})
          blacklist(peer.id, syncConfig.blacklistDuration, "Empty response")
          timers.startSingleTimer(CheckForPeersKey, CheckForPeers, 50.milliseconds)
          context.become(downloading(scheduler, newState))
        case (UsefulData(responsesToProcess), newState) =>
          log.info("Received {} responses from peer {}", responsesToProcess.size, peer.id)
          val currentCapacity =
            ((getFreePeers(newState).size * syncConfig.nodesPerRequest) - newState.nodesToGet.size).max(0)
          scheduler ! MissingNodes(responsesToProcess, currentCapacity)
          // we got free peer lets re-schedule task assignment
          if (newState.nodesToGet.nonEmpty) {
            self ! CheckForPeers
          }
          context.become(downloading(scheduler, newState))
      }

    case PeerRequestHandler.RequestFailed(peer, reason) =>
      context unwatch (sender())
      log.debug(s"Request failed to peer {} due to {}", peer.id, reason)
      timers.startSingleTimer(CheckForPeersKey, CheckForPeers, 50.milliseconds)
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, syncConfig.blacklistDuration, reason)
      }
      context.become(downloading(scheduler, currentState.handleRequestFailure(peer)))

    case RequestTerminated(peer) =>
      log.debug(s"Request to {} terminated", peer.id)
      timers.startSingleTimer(CheckForPeersKey, CheckForPeers, 50.milliseconds)
      context.become(downloading(scheduler, currentState.handleRequestFailure(peer)))
  }

  def idle: Receive = { case RegisterScheduler =>
    log.debug("Scheduler registered, starting sync download")
    context.become(downloading(sender(), DownloaderState(Map.empty, Map.empty)))
  }

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def downloading(scheduler: ActorRef, currentState: DownloaderState): Receive =
    handleRequestResults(scheduler, currentState) orElse
      handleCommonMessages orElse {
        case GetMissingNodes(newNodesToGet) =>
          val freePeers = getFreePeers(currentState)
          if (freePeers.isEmpty) {
            log.info("No available peer, rescheduling request for retrieval")
            timers.startSingleTimer(CheckForPeersKey, CheckForPeers, 50.milliseconds)
            context.become(downloading(scheduler, currentState.scheduleNewNodesForRetrieval(newNodesToGet)))
          } else {
            val (newRequests, newState) =
              currentState.assignTasksToPeers(
                NonEmptyList.fromListUnsafe(freePeers.toList),
                Some(newNodesToGet),
                syncConfig.nodesPerRequest
              )
            log.info(
              "Creating {} new state node requests. Current request queue size is {}",
              newRequests.size,
              newState.nodesToGet.size
            )
            newRequests.foreach { request =>
              requestNodes(request)
            }
            context.become(downloading(scheduler, newState))
          }

        case CheckForPeers =>
          val freePeers = getFreePeers(currentState)
          if (freePeers.isEmpty) {
            log.info("No available peer, rescheduling request for retrieval")
            timers.startSingleTimer(CheckForPeersKey, CheckForPeers, 50.milliseconds)
          } else if (currentState.nodesToGet.isEmpty) {
            log.info("No available work, waiting for ")
          } else {
            val (newRequests, newState) = currentState.assignTasksToPeers(
              NonEmptyList.fromListUnsafe(freePeers.toList),
              None,
              syncConfig.nodesPerRequest
            )
            log.info(
              "Creating {} new state node requests. Current request queue size is {}",
              newRequests.size,
              newState.nodesToGet.size
            )
            newRequests.foreach { request =>
              requestNodes(request)
            }
            context.become(downloading(scheduler, newState))
          }
      }

  override def receive: Receive = idle
}

object SyncStateDownloaderActor {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props = {
    Props(new SyncStateDownloaderActor(etcPeerManager, peerEventBus, syncConfig, scheduler))
  }

  final case object CheckForPeersKey

  final case object CheckForPeers

  final case class RequestTerminated(to: Peer)

  final case class PeerRequest(peer: Peer, nodes: Seq[ByteString])

  final case object RegisterScheduler

  sealed trait ResponseProcessingResult

  case object UnrequestedResponse extends ResponseProcessingResult

  case object NoUsefulDataInResponse extends ResponseProcessingResult

  case class UsefulData(responses: List[SyncResponse]) extends ResponseProcessingResult

  final case class DownloaderState(
      activeRequests: Map[PeerId, Seq[ByteString]],
      nodesToGet: Map[ByteString, Option[PeerId]]
  ) {
    lazy val nonDownloadedNodes = nodesToGet.collect {
      case (hash, maybePeer) if maybePeer.isEmpty => hash
    }.toSeq

    def scheduleNewNodesForRetrieval(nodes: Seq[ByteString]): DownloaderState = {
      val newNodesToGet = nodes.foldLeft(nodesToGet) { case (map, node) =>
        if (map.contains(node)) {
          map
        } else {
          map + (node -> None)
        }
      }

      copy(nodesToGet = newNodesToGet)
    }

    private def addActiveRequest(peerRequest: PeerRequest): DownloaderState = {
      val newNodesToget = peerRequest.nodes.foldLeft(nodesToGet) { case (map, node) =>
        map + (node -> Some(peerRequest.peer.id))
      }

      copy(activeRequests = activeRequests + (peerRequest.peer.id -> peerRequest.nodes), nodesToGet = newNodesToget)
    }

    def handleRequestFailure(from: Peer): DownloaderState = {
      val requestedNodes = activeRequests(from.id)
      val newNodesToGet = requestedNodes.foldLeft(nodesToGet) { case (map, node) =>
        map + (node -> None)
      }

      copy(activeRequests = activeRequests - from.id, nodesToGet = newNodesToGet)
    }

    private def process(
        requested: List[ByteString],
        received: List[ByteString]
    ): (List[ByteString], List[SyncResponse]) = {
      @tailrec
      def go(
          requestedRemaining: List[ByteString],
          receivedRemaining: List[ByteString],
          processed: List[SyncResponse]
      ): (List[ByteString], List[SyncResponse]) = {
        if (requestedRemaining.isEmpty) {
          // we have processed all items
          (List.empty, processed.reverse)
        } else if (receivedRemaining.isEmpty) {
          // there are still some elements which we requested
          (requestedRemaining, processed.reverse)
        } else {
          val nextRequested = requestedRemaining.head
          val nextReceived = receivedRemaining.head
          val receivedHash = kec256(nextReceived)
          if (nextRequested == receivedHash) {
            go(requestedRemaining.tail, receivedRemaining.tail, SyncResponse(receivedHash, nextReceived) :: processed)
          } else {
            // hash of next element does not match return what what we have processed, and remaing hashes to get
            (requestedRemaining, processed)
          }
        }
      }

      go(requested, received, List.empty)
    }

    def handleRequestSuccess(from: Peer, receivedMessage: NodeData): (ResponseProcessingResult, DownloaderState) = {
      activeRequests
        .get(from.id)
        .map { requestedHashes =>
          if (receivedMessage.values.isEmpty) {
            val rescheduleRequestedHashes = requestedHashes.foldLeft(nodesToGet) { case (map, hash) =>
              map + (hash -> None)
            }
            (
              NoUsefulDataInResponse,
              copy(activeRequests = activeRequests - from.id, nodesToGet = rescheduleRequestedHashes)
            )
          } else {
            val (notReceived, received) = process(requestedHashes.toList, receivedMessage.values.toList)
            if (received.isEmpty) {
              val rescheduleRequestedHashes = notReceived.foldLeft(nodesToGet) { case (map, hash) =>
                map + (hash -> None)
              }
              (
                NoUsefulDataInResponse,
                copy(activeRequests = activeRequests - from.id, nodesToGet = rescheduleRequestedHashes)
              )
            } else {
              val afterNotReceive = notReceived.foldLeft(nodesToGet) { case (map, hash) => map + (hash -> None) }
              val afterReceived = received.foldLeft(afterNotReceive) { case (map, received) => map - received.hash }
              (UsefulData(received), copy(activeRequests = activeRequests - from.id, nodesToGet = afterReceived))
            }
          }
        }
        .getOrElse((UnrequestedResponse, this))
    }

    def assignTasksToPeers(
        peers: NonEmptyList[Peer],
        newNodes: Option[Seq[ByteString]],
        nodesPerPeerCapacity: Int
    ): (Seq[PeerRequest], DownloaderState) = {
      def go(
          peersRemaining: List[Peer],
          nodesRemaining: Seq[ByteString],
          createdRequests: List[PeerRequest],
          currentState: DownloaderState
      ): (Seq[PeerRequest], DownloaderState) = {
        if (peersRemaining.isEmpty || nodesRemaining.isEmpty) {
          (createdRequests.reverse, currentState.scheduleNewNodesForRetrieval(nodesRemaining))
        } else {
          val nextPeer = peersRemaining.head
          val (nodes, nodesAfterAssignment) = nodesRemaining.splitAt(nodesPerPeerCapacity)
          val peerRequest = PeerRequest(nextPeer, nodes)
          go(
            peersRemaining.tail,
            nodesAfterAssignment,
            peerRequest :: createdRequests,
            currentState.addActiveRequest(peerRequest)
          )
        }
      }

      val currentNodesToDeliver = newNodes.map(nodes => nonDownloadedNodes ++ nodes).getOrElse(nonDownloadedNodes)
      if (currentNodesToDeliver.isEmpty) {
        (Seq(), this)
      } else {
        go(peers.toList, currentNodesToDeliver, List.empty, this)
      }
    }

  }

}
