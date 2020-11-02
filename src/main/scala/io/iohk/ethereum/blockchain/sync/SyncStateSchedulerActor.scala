package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{
  CriticalError,
  ProcessingStatistics,
  SchedulerState,
  SyncResponse
}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._

class SyncStateSchedulerActor(
    sync: SyncStateScheduler,
    val syncConfig: SyncConfig,
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val scheduler: akka.actor.Scheduler
) extends Actor
    with PeerListSupport
    with BlacklistSupport
    with ActorLogging
    with Timers {

  implicit val monixScheduler = Scheduler(context.dispatcher)

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  private def getFreePeers(state: DownloaderState) = {
    handshakedPeers.collect {
      case (peer, _) if !state.activeRequests.contains(peer.id) && !isBlacklisted(peer.id) => peer
    }
  }

  private def requestNodes(request: PeerRequest): ActorRef = {
    log.info(s"Requesting ${request.nodes.size} from peer ${request.peer}")
    implicit val s = scheduler
    val handler = context.actorOf(
      PeerRequestHandler.props[GetNodeData, NodeData](
        request.peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetNodeData(request.nodes.toList),
        responseMsgCode = NodeData.code
      )
    )
    context.watchWith(handler, RequestTerminated(request.peer))
  }

  def handleRequestResults: Receive = {
    case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
      log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
      context unwatch (sender())
      self ! RequestData(nodeData, peer)

    case PeerRequestHandler.RequestFailed(peer, reason) =>
      context unwatch (sender())
      log.debug(s"Request failed to peer {} due to {}", peer.id, reason)
      self ! RequestFailed(peer, reason)
    case RequestTerminated(peer) =>
      log.debug(s"Request to {} terminated", peer.id)
      self ! RequestFailed(peer, "Peer disconnected in the middle of request")
  }

  val loadingCancelable = sync.loadFilterFromBlockchain.runAsync {
    case Left(value) =>
      log.error(
        "Unexpected error while loading bloom filter. Starting state sync with empty bloom filter" +
          "which may result with degraded performance",
        value
      )
      self ! BloomFilterResult(BloomFilterLoadingResult())
    case Right(value) =>
      log.info("Bloom filter loading finished")
      self ! BloomFilterResult(value)
  }

  def waitingForBloomFilterToLoad(lastReceivedCommand: Option[(SyncStateSchedulerActorCommand, ActorRef)]): Receive =
    handleCommonMessages orElse {
      case BloomFilterResult(result) =>
        log.debug(
          s"Loaded ${result.writtenElements} already known elements from storage to bloom filter the error while loading " +
            s"was ${result.error}"
        )
        lastReceivedCommand match {
          case Some((startSignal: StartSyncingTo, sender)) =>
            val initStats = ProcessingStatistics().addSaved(result.writtenElements)
            val initState = startSyncing(startSignal.stateRoot, startSignal.blockNumber)
            context become (syncing(
              initState,
              DownloaderState(),
              initStats,
              startSignal.blockNumber,
              sender,
              Queue(),
              processing = false,
              None
            ))
          case Some((restartSignal: RestartRequested.type, sender)) =>
            sender ! WaitingForNewTargetBlock
            context.become(idle(ProcessingStatistics().addSaved(result.writtenElements)))
          case _ =>
            context.become(idle(ProcessingStatistics().addSaved(result.writtenElements)))
        }

      case command: SyncStateSchedulerActorCommand =>
        context.become(waitingForBloomFilterToLoad(Some((command, sender()))))
    }

  private def startSyncing(root: ByteString, bn: BigInt): SchedulerState = {
    timers.startTimerAtFixedRate(PrintInfoKey, PrintInfo, 30.seconds)
    log.info(s"Starting state sync to root ${ByteStringUtils.hash2string(root)} on block ${bn}")
    //TODO handle case when we already have root i.e state is synced up to this point
    val initState = sync.initState(root).get
    self ! Sync
    initState
  }

  def idle(processingStatistics: ProcessingStatistics): Receive = handleCommonMessages orElse {
    case StartSyncingTo(root, bn) =>
      val state1 = startSyncing(root, bn)
      context become (syncing(
        state1,
        DownloaderState(),
        processingStatistics,
        bn,
        sender(),
        Queue(),
        processing = false,
        restartRequested = None
      ))
    case PrintInfo =>
      log.info(s"Waiting for target block to start the state sync")
  }

  private def finalizeSync(
      state: SchedulerState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      syncInitiator: ActorRef
  ): Unit = {
    if (state.memBatch.nonEmpty) {
      log.debug(s"Persisting ${state.memBatch.size} elements to blockchain and finalizing the state sync")
      val finalState = sync.persistBatch(state, targetBlock)
      reportStats(syncInitiator, currentStats, finalState)
      syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    } else {
      log.info(s"Finalizing the state sync")
      syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    }
  }

  private def processNodes(
      currentState: SchedulerState,
      currentStats: ProcessingStatistics,
      currentDownloaderState: DownloaderState,
      requestResult: RequestResult
  ): Future[ProcessingResult] = {
    Future {
      requestResult match {
        case RequestData(nodeData, from) =>
          val (resp, newDownloaderState) = currentDownloaderState.handleRequestSuccess(from, nodeData)
          resp match {
            case UnrequestedResponse =>
              ProcessingResult(
                Left(DownloaderError(newDownloaderState, from, "unrequested response", critical = false))
              )
            case NoUsefulDataInResponse =>
              ProcessingResult(
                Left(DownloaderError(newDownloaderState, from, "no useful data in response", critical = true))
              )
            case UsefulData(responses) =>
              sync.processResponses(currentState, responses) match {
                case Left(value) =>
                  ProcessingResult(Left(Critical(value)))
                case Right((newState, stats)) =>
                  ProcessingResult(
                    Right(ProcessingSuccess(newState, newDownloaderState, currentStats.addStats(stats)))
                  )
              }
          }
        case RequestFailed(from, reason) =>
          log.debug("Processing failed request from {}. Failure reason {}", from, reason)
          val newDownloaderState = currentDownloaderState.handleRequestFailure(from)
          ProcessingResult(Left(DownloaderError(newDownloaderState, from, reason, critical = true)))
      }
    }
  }

  private def handleRestart(
      currentState: SchedulerState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      restartRequester: ActorRef
  ): Unit = {
    log.debug("Starting request sequence")
    sync.persistBatch(currentState, targetBlock)
    restartRequester ! WaitingForNewTargetBlock
    context.become(idle(currentStats.addSaved(currentState.memBatch.size)))
  }
  import akka.pattern.pipe

  // scalastyle:off
  def syncing(
      currentState: SchedulerState,
      currentDownloaderState: DownloaderState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      syncInitiator: ActorRef,
      nodesToProcess: Queue[RequestResult],
      processing: Boolean,
      restartRequested: Option[ActorRef]
  ): Receive = handleCommonMessages orElse handleRequestResults orElse {
    case Sync if currentState.numberOfPendingRequests > 0 && restartRequested.isEmpty =>
      val freePeers = getFreePeers(currentDownloaderState)
      nodesToProcess.dequeueOption match {
        case Some((nodes, restOfRequests)) if freePeers.nonEmpty =>
          log.debug(
            "Got {} peer responses to Process, and there are {} idle peers available",
            restOfRequests.size,
            freePeers.size
          )
          val (requests, newState, newDownloaderState) =
            assignTasksToPeers(
              NonEmptyList.fromListUnsafe(freePeers.toList),
              currentState,
              currentDownloaderState,
              syncConfig.nodesPerRequest
            )

          requests.foreach(req => requestNodes(req))
          processNodes(newState, currentStats, newDownloaderState, nodes).pipeTo(self)
          context.become(
            syncing(
              newState,
              newDownloaderState,
              currentStats,
              targetBlock,
              syncInitiator,
              restOfRequests,
              processing,
              restartRequested
            )
          )

        case Some((nodes, restOfRequests)) if freePeers.isEmpty =>
          log.debug(
            "Got {} peer responses to Process, but there are no idle peers to assign new tasks",
            restOfRequests.size
          )
          // we do not have any peers and cannot assign new tasks, but we can still process remaining requests
          processNodes(currentState, currentStats, currentDownloaderState, nodes).pipeTo(self)
          context.become(
            syncing(
              currentState,
              currentDownloaderState,
              currentStats,
              targetBlock,
              syncInitiator,
              restOfRequests,
              processing,
              restartRequested
            )
          )

        case None if freePeers.nonEmpty =>
          log.debug("There no responses to process, but there are {} to assign new tasks", freePeers.size)
          val (requests, newState, newDownloaderState) =
            assignTasksToPeers(
              NonEmptyList.fromListUnsafe(freePeers.toList),
              currentState,
              currentDownloaderState,
              syncConfig.nodesPerRequest
            )

          requests.foreach(req => requestNodes(req))
          context.become(
            syncing(
              newState,
              newDownloaderState,
              currentStats,
              targetBlock,
              syncInitiator,
              nodesToProcess,
              processing = false,
              restartRequested
            )
          )

        case None if freePeers.isEmpty =>
          log.debug(
            "There no responses to process, and no free peers to assign new tasks. There are" +
              "{} active requests in flight",
            currentDownloaderState.activeRequests.size
          )
          if (currentDownloaderState.activeRequests.isEmpty) {
            // we are not processing anything, and there are no free peers and we not waiting for any requests in flight
            // reschedule sync check
            timers.startSingleTimer(SyncKey, Sync, syncConfig.syncRetryInterval)
          }
          context.become(
            syncing(
              currentState,
              currentDownloaderState,
              currentStats,
              targetBlock,
              syncInitiator,
              nodesToProcess,
              processing = false,
              restartRequested
            )
          )
      }

    case Sync if currentState.numberOfPendingRequests > 0 && restartRequested.isDefined =>
      handleRestart(currentState, currentStats, targetBlock, restartRequested.get)

    case Sync if currentState.numberOfPendingRequests == 0 =>
      finalizeSync(currentState, currentStats, targetBlock, syncInitiator)

    case result: RequestResult =>
      if (processing) {
        log.debug(
          "Response received while processing. Enqueuing for import later. Current response queue size: {}",
          nodesToProcess.size + 1
        )
        context.become(
          syncing(
            currentState,
            currentDownloaderState,
            currentStats,
            targetBlock,
            syncInitiator,
            nodesToProcess.enqueue(result),
            processing,
            restartRequested
          )
        )
      } else {
        log.debug("Response received while idle. Initiating response processing")
        processNodes(currentState, currentStats, currentDownloaderState, result).pipeTo(self)
        context.become(
          syncing(
            currentState,
            currentDownloaderState,
            currentStats,
            targetBlock,
            syncInitiator,
            nodesToProcess,
            processing = true,
            restartRequested
          )
        )
      }

    case RestartRequested =>
      log.debug("Received restart request")
      if (processing) {
        log.debug("Received restart while processing. Scheduling it after the task finishes")
        context.become(
          syncing(
            currentState,
            currentDownloaderState,
            currentStats,
            targetBlock,
            syncInitiator,
            nodesToProcess,
            processing,
            restartRequested = Some(sender())
          )
        )
      } else {
        log.debug("Received restart while idle.")
        handleRestart(currentState, currentStats, targetBlock, sender())
      }

    case ProcessingResult(Right(ProcessingSuccess(newState, newDownloaderState, newStats))) =>
      log.debug(
        "Finished processing mpt node batch. Got {} missing nodes. Missing queue has {} elements",
        newState.numberOfPendingRequests,
        newState.numberOfMissingHashes
      )
      val (newState1, newStats1) = if (newState.memBatch.size >= syncConfig.stateSyncPersistBatchSize) {
        log.debug("Current membatch size is {}, persisting nodes to database", newState.memBatch.size)
        (sync.persistBatch(newState, targetBlock), newStats.addSaved(newState.memBatch.size))
      } else {
        (newState, newStats)
      }

      reportStats(syncInitiator, newStats1, newState1)

      context.become(
        syncing(
          newState1,
          newDownloaderState,
          newStats1,
          targetBlock,
          syncInitiator,
          nodesToProcess,
          processing,
          restartRequested
        )
      )
      self ! Sync

    case ProcessingResult(Left(err)) =>
      log.debug("Received error result")
      err match {
        case Critical(er) =>
          log.error(s"Critical error while state syncing ${er}, stopping state sync")
          // TODO we should probably start sync again from new target block, as current trie is malformed or declare
          // fast sync as failure and start normal sync from scratch
          context.stop(self)
        case DownloaderError(newDownloaderState, peer, description, critical) =>
          log.debug("Downloader error by peer {}", peer)
          if (critical && handshakedPeers.contains(peer)) {
            blacklist(peer.id, syncConfig.blacklistDuration, description)
          }
          context.become(
            syncing(
              currentState,
              newDownloaderState,
              currentStats,
              targetBlock,
              syncInitiator,
              nodesToProcess,
              processing,
              restartRequested
            )
          )
          self ! Sync
      }

    case PrintInfo =>
      val syncStats = s""" Status of mpt state sync:
                         | Number of Pending requests: ${currentState.numberOfPendingRequests},
                         | Number of Missing hashes waiting to be retrieved: ${currentState.queue.size()},
                         | Number of Requests waiting for processing: ${nodesToProcess.size},
                         | Number of Mpt nodes saved to database: ${currentStats.saved},
                         | Number of duplicated hashes: ${currentStats.duplicatedHashes},
                         | Number of not requested hashes: ${currentStats.notRequestedHashes},
                        """.stripMargin

      log.info(syncStats)
  }

  override def receive: Receive = waitingForBloomFilterToLoad(None)

  override def postStop(): Unit = {
    loadingCancelable.cancel()
    super.postStop()
  }
}

object SyncStateSchedulerActor {
  case object SyncKey
  case object Sync

  private def reportStats(
      to: ActorRef,
      currentStats: ProcessingStatistics,
      currentState: SyncStateScheduler.SchedulerState
  ): Unit = {
    to ! StateSyncStats(
      currentStats.saved + currentState.memBatch.size,
      currentState.numberOfPendingRequests
    )
  }

  case class StateSyncStats(saved: Long, missing: Long)

  case class ProcessingResult(result: Either[ProcessingError, ProcessingSuccess])

  def props(
      sync: SyncStateScheduler,
      syncConfig: SyncConfig,
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      scheduler: akka.actor.Scheduler
  ): Props = {
    Props(new SyncStateSchedulerActor(sync, syncConfig, etcPeerManager, peerEventBus, scheduler))
  }

  final case object PrintInfo
  final case object PrintInfoKey

  sealed trait SyncStateSchedulerActorCommand
  case class StartSyncingTo(stateRoot: ByteString, blockNumber: BigInt) extends SyncStateSchedulerActorCommand
  case object RestartRequested extends SyncStateSchedulerActorCommand

  sealed trait SyncStateSchedulerActorResponse
  case object StateSyncFinished extends SyncStateSchedulerActorResponse
  case object WaitingForNewTargetBlock extends SyncStateSchedulerActorResponse

  case class GetMissingNodes(nodesToGet: List[ByteString])
  case class MissingNodes(missingNodes: List[SyncResponse], downloaderCapacity: Int)

  case class BloomFilterResult(res: BloomFilterLoadingResult)

  sealed trait RequestResult
  case class RequestData(nodeData: NodeData, from: Peer) extends RequestResult
  case class RequestFailed(from: Peer, reason: String) extends RequestResult

  sealed trait ProcessingError
  case class Critical(er: CriticalError) extends ProcessingError
  case class DownloaderError(newDownloaderState: DownloaderState, by: Peer, description: String, critical: Boolean)
      extends ProcessingError

  case class ProcessingSuccess(
      newSchedulerState: SchedulerState,
      newDownloaderState: DownloaderState,
      processingStats: ProcessingStatistics
  )

  final case class RequestTerminated(to: Peer)

  final case class PeerRequest(peer: Peer, nodes: NonEmptyList[ByteString])

  final case object RegisterScheduler

  sealed trait ResponseProcessingResult

  case object UnrequestedResponse extends ResponseProcessingResult

  case object NoUsefulDataInResponse extends ResponseProcessingResult

  case class UsefulData(responses: List[SyncResponse]) extends ResponseProcessingResult

  final case class DownloaderState(
      activeRequests: Map[PeerId, NonEmptyList[ByteString]],
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
      activeRequests
        .get(from.id)
        .map { requestedNodes =>
          val newNodesToGet = requestedNodes.foldLeft(nodesToGet) { case (map, node) =>
            map + (node -> None)
          }

          copy(activeRequests = activeRequests - from.id, nodesToGet = newNodesToGet)
        }
        .getOrElse(this)
    }

    /**
      * Responses from peers should be delivered in order, but can contain gaps or can be not full, so we cannot fail
      * on first not matching response.
      * Matched responses are returned in correct order, the hashes to be rescheduled are returned in no particular order
      * as they will either way end up in map of hashes to be re-downloaded
      */
    def process(
        requested: NonEmptyList[ByteString],
        received: NonEmptyList[ByteString]
    ): (List[ByteString], List[SyncResponse]) = {
      @tailrec
      def go(
          remainingRequestedHashes: List[ByteString],
          nextResponse: SyncResponse,
          remainingResponses: List[ByteString],
          nonReceivedRequested: List[ByteString],
          processed: List[SyncResponse]
      ): (List[ByteString], List[SyncResponse]) = {
        if (remainingRequestedHashes.isEmpty) {
          (nonReceivedRequested, processed.reverse)
        } else {
          val nextRequestedHash = remainingRequestedHashes.head
          if (nextRequestedHash == nextResponse.hash) {
            if (remainingResponses.isEmpty) {
              val finalNonReceived = remainingRequestedHashes.tail ::: nonReceivedRequested
              val finalProcessed = nextResponse :: processed
              (finalNonReceived, finalProcessed.reverse)
            } else {
              val nexExpectedResponse = SyncResponse(kec256(remainingResponses.head), remainingResponses.head)
              go(
                remainingRequestedHashes.tail,
                nexExpectedResponse,
                remainingResponses.tail,
                nonReceivedRequested,
                nextResponse :: processed
              )
            }
          } else {
            go(
              remainingRequestedHashes.tail,
              nextResponse,
              remainingResponses,
              nextRequestedHash :: nonReceivedRequested,
              processed
            )
          }
        }
      }

      val firstReceivedResponse = SyncResponse(kec256(received.head), received.head)

      go(requested.toList, firstReceivedResponse, received.tail, List.empty, List.empty)
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
            val (notReceived, received) =
              process(requestedHashes, NonEmptyList.fromListUnsafe(receivedMessage.values.toList))
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
      @tailrec
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
          val peerRequest = PeerRequest(nextPeer, NonEmptyList.fromListUnsafe(nodes.toList))
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

  object DownloaderState {
    def apply(): DownloaderState = new DownloaderState(Map.empty, Map.empty)
  }

  def assignTasksToPeers(
      freePeers: NonEmptyList[Peer],
      schedulerState: SchedulerState,
      downloaderState: DownloaderState,
      nodesPerPeer: Int
  ): (Seq[PeerRequest], SchedulerState, DownloaderState) = {
    val retryQueue = downloaderState.nonDownloadedNodes
    val maxNewNodes = ((freePeers.size * nodesPerPeer) - retryQueue.size).max(0)
    val (newNodes, newState) = schedulerState.getMissingHashes(maxNewNodes)
    val (requests, newDownloaderState) =
      downloaderState.assignTasksToPeers(
        NonEmptyList.fromListUnsafe(freePeers.toList),
        Some(newNodes),
        nodesPerPeer
      )
    (requests, newState, newDownloaderState)
  }

}
