package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor.RegisterScheduler
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{ProcessingStatistics, SchedulerState, SyncResponse}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{
  GetMissingNodes,
  MissingNodes,
  PrintInfo,
  PrintInfoKey,
  StartSyncingTo,
  StateSyncFinished,
  maxMembatchSize
}
import io.iohk.ethereum.utils.ByteStringUtils

import scala.concurrent.duration._

class SyncStateSchedulerActor(downloader: ActorRef, sync: SyncStateScheduler)
    extends Actor
    with ActorLogging
    with Timers {

  def idle: Receive = { case StartSyncingTo(root, bn) =>
    timers.startTimerAtFixedRate(PrintInfoKey, PrintInfo, 30.seconds)
    log.info(s"Starting state sync to root ${ByteStringUtils.hash2string(root)} on block ${bn}")
    //TODO handle case when we already have root i.e state is synced up to this point
    val initState = sync.initState(root).get
    val (initialMissing, state1) = initState.getAllMissingHashes
    downloader ! RegisterScheduler
    downloader ! GetMissingNodes(initialMissing)
    context become (syncing(state1, ProcessingStatistics(), bn, sender()))
  }

  private def finalizeSync(state: SchedulerState, targetBlock: BigInt, syncInitiator: ActorRef): Unit = {
    if (state.memBatch.nonEmpty) {
      log.debug(s"Persisting ${state.memBatch.size} elements to blockchain and finalizing the state sync")
      sync.persistBatch(state, targetBlock)
      syncInitiator ! StateSyncFinished
      context.become(idle)
    } else {
      log.info(s"Finalizing the state sync")
      syncInitiator ! StateSyncFinished
      context.become(idle)
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
      // TODO make processing async as sometimes downloader sits idle
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

            if (state2.memBatch.size >= maxMembatchSize) {
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
  }

  override def receive: Receive = idle

}

object SyncStateSchedulerActor {
  def props(downloader: ActorRef, sync: SyncStateScheduler): Props = {
    Props(new SyncStateSchedulerActor(downloader, sync))
  }

  final case object PrintInfo
  final case object PrintInfoKey

  case class StartSyncingTo(stateRoot: ByteString, blockNumber: BigInt)

  case class GetMissingNodes(nodesToGet: List[ByteString])

  case class MissingNodes(missingNodes: List[SyncResponse], downloaderCapacity: Int)

  case object StateSyncFinished

  // TODO determine this number of maybe it should be put to config
  val maxMembatchSize = 10000
}
