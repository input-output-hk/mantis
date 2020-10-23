package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor.{CancelDownload, RegisterScheduler}
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{ProcessingStatistics, SchedulerState, SyncResponse}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{
  BloomFilterResult,
  GetMissingNodes,
  MissingNodes,
  PrintInfo,
  PrintInfoKey,
  RestartRequested,
  StartSyncingTo,
  StateSyncFinished,
  SyncStateSchedulerActorCommand,
  WaitingForNewTargetBlock
}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

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
          context become (syncing(initState, initStats, startSignal.blockNumber, sender))
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
    val (initialMissing, state1) = initState.getAllMissingHashes
    downloader ! RegisterScheduler
    downloader ! GetMissingNodes(initialMissing)
    state1
  }

  def idle(processingStatistics: ProcessingStatistics): Receive = {
    case StartSyncingTo(root, bn) =>
      val state1 = startSyncing(root, bn)
      context become (syncing(state1, processingStatistics, bn, sender()))
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

  // scalastyle:off method.length
  def syncing(
      currentState: SchedulerState,
      currentStats: ProcessingStatistics,
      targetBlock: BigInt,
      syncInitiator: ActorRef
  ): Receive = {
    case MissingNodes(nodes, downloaderCap) =>
      log.debug(s"Received {} new nodes to process", nodes.size)
      // Current SyncStateDownloaderActor makes sure that there is no not requested or duplicated values in its response.
      // so we can ignore those errors.
      //TODO [ETCM-275] process responses asynchronously to avoid steep rise of pending requests after pivot block update
      sync.processResponses(currentState, nodes) match {
        case Left(value) =>
          log.error(s"Critical error while state syncing ${value}, stopping state sync")
          // TODO we should probably start sync again from new target block, as current trie is malformed or declare
          // fast sync as failure and start normal sync from scratch
          context.stop(self)
        case Right((newState, statistics)) =>
          if (newState.numberOfPendingRequests == 0) {
            finalizeSync(newState, targetBlock, syncInitiator)
          } else {
            log.debug(
              s" There are {} pending state requests," +
                s"Missing queue size is {} elements",
              newState.numberOfPendingRequests,
              newState.queue.size()
            )

            val (missing, state2) = sync.getMissingNodes(newState, downloaderCap)

            if (missing.nonEmpty) {
              log.debug(s"Asking downloader for {} missing state nodes", missing.size)
              downloader ! GetMissingNodes(missing)
            }

            if (state2.memBatch.size >= syncConfig.stateSyncPersistBatchSize) {
              log.debug("Current membatch size is {}, persisting nodes to database", state2.memBatch.size)
              val state3 = sync.persistBatch(state2, targetBlock)
              context.become(
                syncing(
                  state3,
                  currentStats.addStats(statistics).addSaved(state2.memBatch.size),
                  targetBlock,
                  syncInitiator
                )
              )
            } else {
              context.become(syncing(state2, currentStats.addStats(statistics), targetBlock, syncInitiator))
            }
          }
      }

    case PrintInfo =>
      val syncStats = s""" Status of mpt state sync:
                             | Number of Pending requests: ${currentState.numberOfPendingRequests},
                             | Number of Missing hashes waiting to be retrieved: ${currentState.queue.size()},
                             | Number of Mpt nodes saved to database: ${currentStats.saved},
                             | Number of duplicated hashes: ${currentStats.duplicatedHashes},
                             | Number of not requested hashes: ${currentStats.notRequestedHashes},
                        """.stripMargin

      log.info(syncStats)

    case RestartRequested =>
      downloader ! CancelDownload
      sync.persistBatch(currentState, targetBlock)
      sender() ! WaitingForNewTargetBlock
      context.become(idle(currentStats))
  }

  override def receive: Receive = waitingForBloomFilterToLoad(None)

  override def postStop(): Unit = {
    loadingCancelable.cancel()
    super.postStop()
  }
}

object SyncStateSchedulerActor {
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
