package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.pattern.pipe
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.fast.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.{
  CriticalError,
  ProcessingStatistics,
  SchedulerState,
  SyncResponse
}
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor._
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, PeerRequestHandler}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.eval.Task
import monix.execution.Scheduler

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
    log.info("Requesting {} from peer {}", request.nodes.size, request.peer)
    implicit val s = scheduler
    val handler = context.actorOf(
      PeerRequestHandler.props[GetNodeData, NodeData](
        request.peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetNodeData(request.nodes.toList),
        responseMsgCode = Codes.NodeDataCode
      )
    )
    context.watchWith(handler, RequestTerminated(request.peer))
  }

  def handleRequestResults: Receive = {
    case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
      log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
      FastSyncMetrics.setMptStateDownloadTime(timeTaken)

      context unwatch (sender())
      self ! RequestData(nodeData, peer)

    case PeerRequestHandler.RequestFailed(peer, reason) =>
      context unwatch (sender())
      log.debug("Request to peer {} failed due to {}", peer.id, reason)
      self ! RequestFailed(peer, reason)
    case RequestTerminated(peer) =>
      log.debug("Request to {} terminated", peer.id)
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
          "Loaded {} already known elements from storage to bloom filter the error while loading was {}",
          result.writtenElements,
          result.error
        )
        lastReceivedCommand match {
          case Some((startSignal: StartSyncingTo, sender)) =>
            val initStats = ProcessingStatistics().addSaved(result.writtenElements)
            startSyncing(startSignal.stateRoot, startSignal.blockNumber, initStats, sender)
          case Some((RestartRequested, sender)) =>
            // TODO: are we testing this path?
            sender ! WaitingForNewTargetBlock
            context.become(idle(ProcessingStatistics().addSaved(result.writtenElements)))
          case _ =>
            context.become(idle(ProcessingStatistics().addSaved(result.writtenElements)))
        }

      case command: SyncStateSchedulerActorCommand =>
        context.become(waitingForBloomFilterToLoad(Some((command, sender()))))
    }

  private def startSyncing(
      root: ByteString,
      bn: BigInt,
      initialStats: ProcessingStatistics,
      initiator: ActorRef
  ): Unit = {
    timers.startTimerAtFixedRate(PrintInfoKey, PrintInfo, 30.seconds)
    log.info("Starting state sync to root {} on block {}", ByteStringUtils.hash2string(root), bn)
    //TODO handle case when we already have root i.e state is synced up to this point
    val initState = sync.initState(root).get
    context become syncing(
      SyncSchedulerActorState.initial(initState, initialStats, bn, initiator)
    )
    self ! Sync
  }

  def idle(processingStatistics: ProcessingStatistics): Receive = handleCommonMessages orElse {
    case StartSyncingTo(root, bn) =>
      startSyncing(root, bn, processingStatistics, sender())
    case PrintInfo =>
      log.info("Waiting for target block to start the state sync")
  }

  private def finalizeSync(
      state: SyncSchedulerActorState
  ): Unit = {
    if (state.memBatch.nonEmpty) {
      log.debug("Persisting {} elements to blockchain and finalizing the state sync", state.memBatch.size)
      val finalState = sync.persistBatch(state.currentSchedulerState, state.targetBlock)
      reportStats(state.syncInitiator, state.currentStats.addSaved(state.memBatch.size), finalState)
      state.syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    } else {
      log.info("Finalizing the state sync")
      state.syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    }
  }

  private def processNodes(
      currentState: SyncSchedulerActorState,
      requestResult: RequestResult
  ): ProcessingResult = {
    requestResult match {
      case RequestData(nodeData, from) =>
        val (resp, newDownloaderState) = currentState.currentDownloaderState.handleRequestSuccess(from, nodeData)
        resp match {
          case UnrequestedResponse =>
            ProcessingResult(
              Left(DownloaderError(newDownloaderState, from, "unrequested response", blacklistPeer = false))
            )
          case NoUsefulDataInResponse =>
            ProcessingResult(
              Left(DownloaderError(newDownloaderState, from, "no useful data in response", blacklistPeer = true))
            )
          case UsefulData(responses) =>
            sync.processResponses(currentState.currentSchedulerState, responses) match {
              case Left(value) =>
                ProcessingResult(Left(Critical(value)))
              case Right((newState, stats)) =>
                ProcessingResult(
                  Right(ProcessingSuccess(newState, newDownloaderState, currentState.currentStats.addStats(stats)))
                )
            }
        }
      case RequestFailed(from, reason) =>
        log.debug("Processing failed request from {}. Failure reason {}", from, reason)
        val newDownloaderState = currentState.currentDownloaderState.handleRequestFailure(from)
        ProcessingResult(Left(DownloaderError(newDownloaderState, from, reason, blacklistPeer = true)))
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

  // scalastyle:off cyclomatic.complexity method.length
  def syncing(currentState: SyncSchedulerActorState): Receive =
    handleCommonMessages orElse handleRequestResults orElse {
      case Sync if currentState.hasRemainingPendingRequests && !currentState.restartHasBeenRequested =>
        val freePeers = getFreePeers(currentState.currentDownloaderState).toList
        (currentState.getRequestToProcess, NonEmptyList.fromList(freePeers)) match {
          case (Some((nodes, newState)), Some(peers)) =>
            log.debug(
              "Got {} peer responses remaining to process, and there are {} idle peers available",
              newState.numberOfRemainingRequests,
              peers.size
            )
            val (requests, newState1) = newState.assignTasksToPeers(peers, syncConfig.nodesPerRequest)
            requests.foreach(req => requestNodes(req))
            Task(processNodes(newState1, nodes)).runToFuture.pipeTo(self)
            context.become(syncing(newState1))

          case (Some((nodes, newState)), None) =>
            log.debug(
              "Got {} peer responses remaining to process, but there are no idle peers to assign new tasks",
              newState.numberOfRemainingRequests
            )
            // we do not have any peers and cannot assign new tasks, but we can still process remaining requests
            Task(processNodes(newState, nodes)).runToFuture.pipeTo(self)
            context.become(syncing(newState))

          case (None, Some(peers)) =>
            log.debug("There no responses to process, but there are {} free peers to assign new tasks", peers.size)
            val (requests, newState) = currentState.assignTasksToPeers(peers, syncConfig.nodesPerRequest)
            requests.foreach(req => requestNodes(req))
            context.become(syncing(newState.finishProcessing))

          case (None, None) =>
            log.debug(
              "There no responses to process, and no free peers to assign new tasks. There are" +
                "{} active requests in flight",
              currentState.activePeerRequests.size
            )
            if (currentState.activePeerRequests.isEmpty) {
              // we are not processing anything, and there are no free peers and we not waiting for any requests in flight
              // reschedule sync check
              timers.startSingleTimer(SyncKey, Sync, syncConfig.syncRetryInterval)
            }
            context.become(syncing(currentState.finishProcessing))
        }

      case Sync if currentState.hasRemainingPendingRequests && currentState.restartHasBeenRequested =>
        handleRestart(
          currentState.currentSchedulerState,
          currentState.currentStats,
          currentState.targetBlock,
          currentState.restartRequested.get
        )

      case Sync if !currentState.hasRemainingPendingRequests =>
        finalizeSync(currentState)

      case result: RequestResult =>
        if (currentState.isProcessing) {
          log.debug(
            "Response received while processing. Enqueuing for import later. Current response queue size: {}",
            currentState.nodesToProcess.size + 1
          )
          context.become(syncing(currentState.withNewRequestResult(result)))
        } else {
          log.debug("Response received while idle. Initiating response processing")
          val newState = currentState.initProcessing
          Task(processNodes(newState, result)).runToFuture.pipeTo(self)
          context.become(syncing(newState))
        }

      case RestartRequested =>
        log.debug("Received restart request")
        if (currentState.isProcessing) {
          log.debug("Received restart while processing. Scheduling it after the task finishes")
          context.become(syncing(currentState.withRestartRequested(sender())))
        } else {
          log.debug("Received restart while idle.")
          handleRestart(
            currentState.currentSchedulerState,
            currentState.currentStats,
            currentState.targetBlock,
            sender()
          )
        }

      case ProcessingResult(Right(ProcessingSuccess(newState, newDownloaderState, newStats))) =>
        log.debug(
          "Finished processing mpt node batch. Got {} missing nodes. Missing queue has {} elements",
          newState.numberOfPendingRequests,
          newState.numberOfMissingHashes
        )
        val (newState1, newStats1) = if (newState.memBatch.size >= syncConfig.stateSyncPersistBatchSize) {
          log.debug("Current membatch size is {}, persisting nodes to database", newState.memBatch.size)
          (sync.persistBatch(newState, currentState.targetBlock), newStats.addSaved(newState.memBatch.size))
        } else {
          (newState, newStats)
        }

        reportStats(currentState.syncInitiator, newStats1, newState1)
        context.become(syncing(currentState.withNewProcessingResults(newState1, newDownloaderState, newStats1)))
        self ! Sync

      case ProcessingResult(Left(err)) =>
        log.debug("Received error result")
        err match {
          case Critical(er) =>
            log.error("Critical error while state syncing {}, stopping state sync", er)
            // TODO we should probably start sync again from new target block, as current trie is malformed or declare
            // fast sync as failure and start normal sync from scratch
            context.stop(self)
          case DownloaderError(newDownloaderState, peer, description, blacklistPeer) =>
            log.debug("Downloader error by peer {}", peer)
            if (blacklistPeer && handshakedPeers.contains(peer)) {
              blacklist(peer.id, syncConfig.blacklistDuration, description)
            }
            context.become(syncing(currentState.withNewDownloaderState(newDownloaderState)))
            self ! Sync
        }

      case PrintInfo =>
        log.info("{}", currentState)
    }

  override def receive: Receive = waitingForBloomFilterToLoad(None)

  override def postStop(): Unit = {
    loadingCancelable.cancel()
    super.postStop()
  }
}

// scalastyle:off number.of.methods
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
  case class DownloaderError(newDownloaderState: DownloaderState, by: Peer, description: String, blacklistPeer: Boolean)
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
}
