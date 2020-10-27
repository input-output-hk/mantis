package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor.{CancelDownload, RegisterScheduler}
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{
  CriticalError,
  ProcessingStatistics,
  SchedulerState,
  SyncResponse
}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{
  BloomFilterResult,
  GetMissingNodes,
  MissingNodes,
  PrintInfo,
  PrintInfoKey,
  ProcessingResult,
  RestartRequested,
  StartSyncingTo,
  StateSyncFinished,
  Sync,
  SyncStateSchedulerActorCommand,
  WaitingForNewTargetBlock
}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._

class SyncStateSchedulerActor(downloader: ActorRef, sync: SyncStateScheduler, syncConfig: SyncConfig)
    extends Actor
    with ActorLogging
    with Timers {
  implicit val scheduler = Scheduler(context.dispatcher)

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

  def waitingForBloomFilterToLoad(lastReceivedCommand: Option[(SyncStateSchedulerActorCommand, ActorRef)]): Receive = {
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
            initStats,
            startSignal.blockNumber,
            sender,
            1,
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
    downloader ! RegisterScheduler
    self ! Sync
    initState
  }

  def idle(processingStatistics: ProcessingStatistics): Receive = {
    case StartSyncingTo(root, bn) =>
      val state1 = startSyncing(root, bn)
      context become (syncing(
        state1,
        processingStatistics,
        bn,
        sender(),
        1,
        Queue(),
        processing = false,
        restartRequested = None
      ))
    case PrintInfo =>
      log.info(s"Waiting for target block to start the state sync")
  }

  private def finalizeSync(state: SchedulerState, targetBlock: BigInt, syncInitiator: ActorRef): Unit = {
    if (state.memBatch.nonEmpty) {
      log.debug(s"Persisting ${state.memBatch.size} elements to blockchain and finalizing the state sync")
      sync.persistBatch(state, targetBlock)
      syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    } else {
      log.info(s"Finalizing the state sync")
      syncInitiator ! StateSyncFinished
      context.become(idle(ProcessingStatistics()))
    }
  }

  import akka.pattern.pipe
  private def processNodes(currentState: SchedulerState, nodes: List[SyncResponse]): Future[ProcessingResult] = {
    Future(sync.processResponses(currentState, nodes)).map(ProcessingResult).pipeTo(self)
  }

  private def handleRestart(
      currentState: SchedulerState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      restartRequester: ActorRef
  ): Unit = {
    log.debug("Starting request sequence")
    downloader ! CancelDownload
    sync.persistBatch(currentState, targetBlock)
    restartRequester ! WaitingForNewTargetBlock
    context.become(idle(currentStats.addSaved(currentState.memBatch.size)))
  }

  // scalastyle:off method.length cyclomatic.complexity
  def syncing(
      currentState: SchedulerState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      syncInitiator: ActorRef,
      currentDownloaderCapacity: Int,
      nodesToProcess: Queue[List[SyncResponse]],
      processing: Boolean,
      restartRequested: Option[ActorRef]
  ): Receive = {
    case Sync if currentState.numberOfPendingRequests > 0 && restartRequested.isEmpty =>
      val (newCapacity, newState) = if (currentDownloaderCapacity > 0) {
        val (missingNodes, newState) = sync.getMissingNodes(currentState, currentDownloaderCapacity)
        downloader ! GetMissingNodes(missingNodes)
        (currentDownloaderCapacity - missingNodes.size, newState)
      } else {
        (0, currentState)
      }

      nodesToProcess.dequeueOption match {
        case Some((nodesToProcess, restOfNodes)) =>
          log.debug("Start processing next batch of mpt nodes. There are {} requests left", restOfNodes.size)
          processNodes(newState, nodesToProcess)
          context.become(
            syncing(
              newState,
              currentStats,
              targetBlock,
              syncInitiator,
              newCapacity,
              restOfNodes,
              processing,
              restartRequested
            )
          )

        case None =>
          log.debug("No more mpt nodes to process.")
          context.become(
            syncing(
              newState,
              currentStats,
              targetBlock,
              syncInitiator,
              newCapacity,
              nodesToProcess,
              processing = false,
              restartRequested
            )
          )
      }

    case Sync if currentState.numberOfPendingRequests > 0 && restartRequested.isDefined =>
      handleRestart(currentState, currentStats, targetBlock, restartRequested.get)

    case Sync if currentState.numberOfPendingRequests == 0 =>
      finalizeSync(currentState, targetBlock, syncInitiator)

    case MissingNodes(nodes, downloaderCap) =>
      log.debug(s"Received {} new nodes to process. Downloader capacity is {}", nodes.size, downloaderCap)
      if (processing) {
        log.debug(
          s"Nodes received while processing. Enqueuing for import later. Current request queue size: {}",
          nodesToProcess.size + 1
        )
        context.become(
          syncing(
            currentState,
            currentStats,
            targetBlock,
            syncInitiator,
            downloaderCap,
            nodesToProcess.enqueue(nodes),
            processing,
            restartRequested
          )
        )
      } else {
        log.debug(s"Nodes received while idle. Init mpt nodes processing")
        processNodes(currentState, nodes)
        context.become(
          syncing(
            currentState,
            currentStats,
            targetBlock,
            syncInitiator,
            downloaderCap,
            nodesToProcess,
            processing = true,
            restartRequested = restartRequested
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
            currentStats,
            targetBlock,
            syncInitiator,
            currentDownloaderCapacity,
            nodesToProcess,
            processing,
            restartRequested = Some(sender())
          )
        )
      } else {
        log.debug("Received restart while idle.")
        handleRestart(currentState, currentStats, targetBlock, sender())
      }

    case ProcessingResult(Right((state, stats))) =>
      log.debug(
        "Finished processing mpt node batch. Got {} missing nodes. Missing queue has {} elements",
        state.numberOfPendingRequests,
        state.numberOfMissingHashes
      )
      val (state1, stats1) = if (state.memBatch.size >= syncConfig.stateSyncPersistBatchSize) {
        log.debug("Current membatch size is {}, persisting nodes to database", state.memBatch.size)
        (sync.persistBatch(state, targetBlock), currentStats.addStats(stats).addSaved(state.memBatch.size))
      } else {
        (state, currentStats.addStats(stats))
      }
      context.become(
        syncing(
          state1,
          stats1,
          targetBlock,
          syncInitiator,
          currentDownloaderCapacity,
          nodesToProcess,
          processing,
          restartRequested
        )
      )
      self ! Sync

    case ProcessingResult(Left(criticalError)) =>
      log.error(s"Critical error while state syncing ${criticalError}, stopping state sync")
      // TODO we should probably start sync again from new target block, as current trie is malformed or declare
      // fast sync as failure and start normal sync from scratch
      context.stop(self)

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
  case object Sync
  case class ProcessingResult(result: Either[CriticalError, (SchedulerState, ProcessingStatistics)])

  def props(downloader: ActorRef, sync: SyncStateScheduler, syncConfig: SyncConfig): Props = {
    Props(new SyncStateSchedulerActor(downloader, sync, syncConfig))
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

}
