package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.validators.Validators

import scala.annotation.tailrec

class SyncController(
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    fastSyncStateStorage: FastSyncStateStorage,
    ledger: Ledger,
    validators: Validators,
    peerEventBus: ActorRef,
    pendingTransactionsManager: ActorRef,
    ommersPool: ActorRef,
    etcPeerManager: ActorRef,
    syncConfig: SyncConfig,
    shutdownAppFn: () => Unit,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor
    with ActorLogging {

  import SyncController._

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override def receive: Receive = idle

  def idle: Receive = {
    case Start => start()
  }

  def runningFastSync(fastSync: ActorRef): Receive = {
    case FastSync.Done =>
      fastSync ! PoisonPill
      startRegularSync()

    case other => fastSync.forward(other)
  }

  def runningRegularSync(regularSync: ActorRef): Receive = {
    case other => regularSync.forward(other)
  }

  def start(): Unit = {
    // import syncConfig.doFastSync

    val bestBlock = appStateStorage.getBestBlockNumber()
    val bestBlockHeader = blockchain.getBlockByNumber(bestBlock).get.header
    val missingBlockBodies = collectMissingBlocks(bestBlock)
    println(s"Starting fast sync. Found ${missingBlockBodies.length} missing block bodies to download.")
    val syncState = SyncState(bestBlockHeader, Nil, Nil, missingBlockBodies, Nil, 0, bestBlock)
    fastSyncStateStorage.putSyncState(syncState)
    startFastSync()

    /*

    appStateStorage.putSyncStartingBlock(appStateStorage.getBestBlockNumber())
    (appStateStorage.isFastSyncDone(), doFastSync) match {
      case (false, true) =>
        startFastSync()
      case (true, true) =>
        log.warning(s"do-fast-sync is set to $doFastSync but fast sync cannot start because it has already been completed")
        startRegularSync()
      case (true, false) =>
        startRegularSync()
      case (false, false) =>
        //Check whether fast sync was started before
        if (fastSyncStateStorage.getSyncState().isDefined) {
          log.warning(s"do-fast-sync is set to $doFastSync but regular sync cannot start because fast sync hasn't completed")
          startFastSync()
        } else
          startRegularSync()
    }
    */
  }

  @tailrec
  private def collectMissingBlocks(until: BigInt, i: BigInt = 0, ret: Seq[ByteString] = Nil): Seq[ByteString] = {
    if (until == i) {
      ret
    } else {
      if (i % 10000 == 0) println(s"Collecting missing bodies from $i, so far found ${ret.length} missing bodies")
      val header = blockchain.getBlockHeaderByNumber(i).get
      if (blockchain.getBlockBodyByHash(header.hash).isEmpty) collectMissingBlocks(until, i + 1, ret :+ header.hash)
      else collectMissingBlocks(until, i + 1, ret)
    }
  }

  def startFastSync(): Unit = {
    val fastSync = context.actorOf(FastSync.props(fastSyncStateStorage, appStateStorage, blockchain, validators,
      peerEventBus, etcPeerManager, syncConfig, scheduler), "fast-sync")
    fastSync ! FastSync.Start
    context become runningFastSync(fastSync)
  }

  def startRegularSync(): Unit = {
    val regularSync = context.actorOf(RegularSync.props(appStateStorage, etcPeerManager,
      peerEventBus, ommersPool, pendingTransactionsManager, new BlockBroadcast(etcPeerManager),
      ledger, syncConfig, scheduler), "regular-sync")
    regularSync ! RegularSync.Start
    context become runningRegularSync(regularSync)
  }
}

object SyncController {
  // scalastyle:off parameter.number
  def props(appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            syncStateStorage: FastSyncStateStorage,
            ledger: Ledger,
            validators: Validators,
            peerEventBus: ActorRef,
            pendingTransactionsManager: ActorRef,
            ommersPool: ActorRef,
            etcPeerManager: ActorRef,
            syncConfig: SyncConfig,
            shutdownFn: () => Unit):
  Props = Props(new SyncController(appStateStorage, blockchain, syncStateStorage, ledger, validators,
    peerEventBus, pendingTransactionsManager, ommersPool, etcPeerManager, syncConfig, shutdownFn))

  case object Start
}
