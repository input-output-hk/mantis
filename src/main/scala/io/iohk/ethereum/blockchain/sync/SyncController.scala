package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Scheduler}
import io.iohk.ethereum.blockchain.sync.fast.FastSync
import io.iohk.ethereum.blockchain.sync.regular.RegularSync
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.ledger.BlockImport

class SyncController(
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    fastSyncStateStorage: FastSyncStateStorage,
    blockImport: BlockImport,
    validators: Validators,
    peerEventBus: ActorRef,
    pendingTransactionsManager: ActorRef,
    ommersPool: ActorRef,
    etcPeerManager: ActorRef,
    blacklist: Blacklist,
    syncConfig: SyncConfig,
    externalSchedulerOpt: Option[Scheduler] = None
) extends Actor
    with ActorLogging {

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override def receive: Receive = idle

  def idle: Receive = { case SyncProtocol.Start =>
    start()
  }

  def runningFastSync(fastSync: ActorRef): Receive = {
    case FastSync.Done =>
      fastSync ! PoisonPill
      startRegularSync()

    case other => fastSync.forward(other)
  }

  def runningRegularSync(regularSync: ActorRef): Receive = { case other =>
    regularSync.forward(other)
  }

  def start(): Unit = {
    import syncConfig.doFastSync

    appStateStorage.putSyncStartingBlock(appStateStorage.getBestBlockNumber())
    (appStateStorage.isFastSyncDone(), doFastSync) match {
      case (false, true) =>
        startFastSync()
      case (true, true) =>
        log.warning(
          s"do-fast-sync is set to $doFastSync but fast sync cannot start because it has already been completed"
        )
        startRegularSync()
      case (true, false) =>
        startRegularSync()
      case (false, false) =>
        //Check whether fast sync was started before
        if (fastSyncStateStorage.getSyncState().isDefined) {
          log.warning(
            s"do-fast-sync is set to $doFastSync but regular sync cannot start because fast sync hasn't completed"
          )
          startFastSync()
        } else
          startRegularSync()
    }
  }

  def startFastSync(): Unit = {
    val fastSync = context.actorOf(
      FastSync.props(
        fastSyncStateStorage,
        appStateStorage,
        blockchain,
        validators,
        peerEventBus,
        etcPeerManager,
        blacklist,
        syncConfig,
        scheduler
      ),
      "fast-sync"
    )
    fastSync ! SyncProtocol.Start
    context become runningFastSync(fastSync)
  }

  def startRegularSync(): Unit = {
    val peersClient =
      context.actorOf(PeersClient.props(etcPeerManager, peerEventBus, blacklist, syncConfig, scheduler), "peers-client")
    val regularSync = context.actorOf(
      RegularSync.props(
        peersClient,
        etcPeerManager,
        peerEventBus,
        blockImport,
        blockchain,
        validators.blockValidator,
        blacklist,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        scheduler
      ),
      "regular-sync"
    )
    regularSync ! SyncProtocol.Start
    context become runningRegularSync(regularSync)
  }

}

object SyncController {
  // scalastyle:off parameter.number
  def props(
      appStateStorage: AppStateStorage,
      blockchain: Blockchain,
      syncStateStorage: FastSyncStateStorage,
      blockImport: BlockImport,
      validators: Validators,
      peerEventBus: ActorRef,
      pendingTransactionsManager: ActorRef,
      ommersPool: ActorRef,
      etcPeerManager: ActorRef,
      blacklist: Blacklist,
      syncConfig: SyncConfig
  ): Props =
    Props(
      new SyncController(
        appStateStorage,
        blockchain,
        syncStateStorage,
        blockImport,
        validators,
        peerEventBus,
        pendingTransactionsManager,
        ommersPool,
        etcPeerManager,
        blacklist,
        syncConfig
      )
    )
}
