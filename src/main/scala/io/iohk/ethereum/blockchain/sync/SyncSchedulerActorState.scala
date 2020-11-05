package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorRef
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{ProcessingStatistics, SchedulerState}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{PeerRequest, RequestResult}
import io.iohk.ethereum.network.{Peer, PeerId}

import scala.collection.immutable.Queue

case class SyncSchedulerActorState(
    currentSchedulerState: SchedulerState,
    currentDownloaderState: DownloaderState,
    currentStats: ProcessingStatistics,
    targetBlock: BigInt,
    syncInitiator: ActorRef,
    nodesToProcess: Queue[RequestResult],
    processing: Boolean,
    restartRequested: Option[ActorRef]
) {
  def hasRemainingPendingRequests: Boolean = currentSchedulerState.numberOfPendingRequests > 0
  def isProcessing: Boolean = processing
  def restartHasBeenRequested: Boolean = restartRequested.isDefined
  def withNewRequestResult(requestResult: RequestResult): SyncSchedulerActorState =
    copy(nodesToProcess = nodesToProcess.enqueue(requestResult))

  def withNewProcessingResults(
      newSchedulerState: SchedulerState,
      newDownloaderState: DownloaderState,
      newStats: ProcessingStatistics
  ): SyncSchedulerActorState = {
    copy(
      currentSchedulerState = newSchedulerState,
      currentDownloaderState = newDownloaderState,
      currentStats = newStats
    )
  }

  def withNewDownloaderState(newDownloaderState: DownloaderState): SyncSchedulerActorState = {
    copy(currentDownloaderState = newDownloaderState)
  }

  def withRestartRequested(restartRequester: ActorRef): SyncSchedulerActorState = {
    copy(restartRequested = Some(restartRequester))
  }

  def initProcessing: SyncSchedulerActorState = {
    copy(processing = true)
  }

  def finishProcessing: SyncSchedulerActorState = {
    copy(processing = false)
  }

  def assignTasksToPeers(
      freePeers: NonEmptyList[Peer],
      nodesPerPeer: Int
  ): (Seq[PeerRequest], SyncSchedulerActorState) = {
    val retryQueue = currentDownloaderState.nonDownloadedNodes
    val maxNewNodes = ((freePeers.size * nodesPerPeer) - retryQueue.size).max(0)
    val (newNodes, newState) = currentSchedulerState.getMissingHashes(maxNewNodes)
    val (requests, newDownloaderState) =
      currentDownloaderState.assignTasksToPeers(
        NonEmptyList.fromListUnsafe(freePeers.toList),
        Some(newNodes),
        nodesPerPeer
      )
    (requests, copy(currentSchedulerState = newState, currentDownloaderState = newDownloaderState))
  }

  def getRequestToProcess: Option[(RequestResult, SyncSchedulerActorState)] = {
    nodesToProcess.dequeueOption.map { case (result, restOfResults) =>
      (result, copy(nodesToProcess = restOfResults))
    }
  }

  def numberOfRemainingRequests: Int = nodesToProcess.size

  def memBatch: Map[ByteString, (ByteString, SyncStateScheduler.RequestType)] = currentSchedulerState.memBatch

  def activePeerRequests: Map[PeerId, NonEmptyList[ByteString]] = currentDownloaderState.activeRequests

  override def toString: String = {
    s""" Status of mpt state sync:
       | Number of Pending requests: ${currentSchedulerState.numberOfPendingRequests},
       | Number of Missing hashes waiting to be retrieved: ${currentSchedulerState.queue.size()},
       | Number of Requests waiting for processing: ${nodesToProcess.size},
       | Number of Mpt nodes saved to database: ${currentStats.saved},
       | Number of duplicated hashes: ${currentStats.duplicatedHashes},
       | Number of not requested hashes: ${currentStats.notRequestedHashes},
       | Number of active peer requests: ${currentDownloaderState.activeRequests.size}
                        """.stripMargin
  }
}

object SyncSchedulerActorState {
  def initial(
      initialSchedulerState: SchedulerState,
      initialStats: ProcessingStatistics,
      targetBlock: BigInt,
      syncInitiator: ActorRef
  ): SyncSchedulerActorState = {
    SyncSchedulerActorState(
      initialSchedulerState,
      DownloaderState(),
      initialStats,
      targetBlock,
      syncInitiator,
      Queue(),
      processing = false,
      restartRequested = None
    )

  }
}
