package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor.RegisterScheduler
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{SchedulerState, SyncResponse}
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{GetMissingNodes, MissingNodes, StartSyncingTo, StateSyncFinished, maxMembatchSize}
import io.iohk.ethereum.utils.ByteStringUtils

class SyncStateSchedulerActor(downloader: ActorRef, sync: SyncStateScheduler) extends Actor with ActorLogging {

  def idle: Receive = {
    case StartSyncingTo(root, bn) =>
      log.info(s"Starting state sync to root ${ByteStringUtils.hash2string(root)} on block ${bn}")
      //TODO handle case when we already have root i.e state is synced up to this point
      val initState = sync.initState(root).get
      val (initialMissing, state1) = initState.getAllMissingHashes
      downloader ! RegisterScheduler
      downloader ! GetMissingNodes(initialMissing)
      context become (syncing(state1, bn, sender()))
  }

  private def finalizeSync(state: SchedulerState, targetBlock: BigInt, syncInitiator: ActorRef): Unit = {
    if (state.memBatch.nonEmpty) {
      log.info(s"Persisting ${state.memBatch.size} elements to blockchain and finalizing the state sync")
      sync.persistBatch(state, targetBlock)
      syncInitiator ! StateSyncFinished
      context.become(idle)
    } else {
      log.info(s"Finalizing the state sync")
      syncInitiator ! StateSyncFinished
      context.become(idle)
    }
  }

  def syncing(currentState: SchedulerState, targetBlock: BigInt, syncInitiator: ActorRef): Receive = {
    case MissingNodes(nodes, downloaderCap) =>
      log.info(s"Received {} new nodes to process", nodes.size)
      // Current SyncStateDownloaderActor makes sure that there is no not requested or duplicated values in its response.
      // so we can ignore those errors.
      sync.processResponses(currentState, nodes) match {
        case Left(value) =>
          log.info(s"Critical error while state syncing ${value}, stopping state sync")
          // TODO we should probably start sync again from new target block, as current trie is malformed or declare
          // fast sync as failure and start normal sync from scratch
          context.stop(self)
        case Right(newState) =>
          if (newState.numberOfPendingRequests == 0) {
            finalizeSync(newState, targetBlock, syncInitiator)
          } else {
            log.info(s" There are {} pending state requests," +
              s"Missing queue size is {} elements", newState.numberOfPendingRequests, newState.queue.size())

            val (missing, state2) = sync.getMissingNodes(newState, downloaderCap)

            if (missing.nonEmpty) {
              log.debug(s"Asking downloader for {} missing state nodes", missing.size)
              downloader ! GetMissingNodes(missing)
            }

            if (state2.memBatch.size >= maxMembatchSize) {
              log.info("Current membatch size is {}, persisting nodes to database", state2.memBatch.size)
              val state3 = sync.persistBatch(state2, targetBlock)
              context.become(syncing(state3, targetBlock, syncInitiator))
            } else {
              context.become(syncing(state2, targetBlock, syncInitiator))
            }
          }

      }
  }

  override def receive: Receive = idle

}

object SyncStateSchedulerActor {
  def props(downloader: ActorRef, sync: SyncStateScheduler): Props = {
    Props(new SyncStateSchedulerActor(downloader, sync))
  }

  case class StartSyncingTo(stateRoot: ByteString, blockNumber: BigInt)

  case class GetMissingNodes(nodesToGet: List[ByteString])

  case class MissingNodes(missingNodes: List[SyncResponse], downloaderCapacity: Int)

  case object StateSyncFinished

  // TODO determine this number of maybe it should be put to config
  val maxMembatchSize = 10000
}
