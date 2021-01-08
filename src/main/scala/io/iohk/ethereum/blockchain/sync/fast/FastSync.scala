package io.iohk.ethereum.blockchain.sync.fast

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig

class FastSync(
    fastSyncStateStorage: FastSyncStateStorage,
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    validators: Validators,
    peerEventBus: ActorRef,
    etcPeerManager: ActorRef,
    syncConfig: SyncConfig,
    scheduler: Scheduler
) extends Actor with ActorLogging {

  import FastSync._

  val syncController: ActorRef = context.parent
  val pivotBlockSelector: ActorRef = context.actorOf(
    PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self),
    "pivot-block-selector",
  )
  val syncStateStorageActor = context.actorOf(Props[StateStorageActor], "state-storage")
  val syncStateScheduler = context.actorOf(
    SyncStateSchedulerActor
      .props(
        SyncStateScheduler(blockchain, syncConfig.stateSyncBloomFilterSize),
        syncConfig,
        etcPeerManager,
        peerEventBus,
        scheduler
      ),
    "state-scheduler"
  )

  syncStateStorageActor ! fastSyncStateStorage

  override def receive: Receive = idle

  def idle: Receive = {
    case SyncProtocol.Start =>
      start()
    case SyncProtocol.GetStatus =>
      sender() ! SyncProtocol.Status.NotSyncing
  }

  def start(): Unit = {
    log.info("Trying to start block synchronization (fast mode)")
    fastSyncStateStorage.getSyncState() match {
      case Some(syncState) => startWithState(syncState)
      case None => startFromScratch()
    }
  }

  def startWithState(syncState: SyncState): Unit = {
    log.info(s"Starting with existing state and asking for new pivot block")
    val syncingHandler = context.actorOf(syncingHandlerProps(syncState))
    syncingHandler ! SyncingHandler.AskForPivotBlockUpdate(SyncRestart)
    context become handlingSync(syncingHandler)
  }

  private def syncingHandlerProps(syncState: SyncState) = {
    val storage = new SyncingHandlerStorage(syncStateStorageActor, syncStateScheduler, appStateStorage, blockchain)
    val validator = new FastSyncValidator(blockchain, validators)
    SyncingHandler.props(
      syncState,
      storage,
      validator,
      peerEventBus,
      etcPeerManager,
      pivotBlockSelector,
      syncConfig,
      scheduler
    )
  }

  def startFromScratch(): Unit = {
    pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
    context become waitingForPivotBlock
  }

  def waitingForPivotBlock: Receive = {
    case SyncProtocol.GetStatus => sender() ! SyncProtocol.Status.NotSyncing
    case PivotBlockSelector.Result(pivotBlockHeader) =>
      if (pivotBlockHeader.number < 1) {
        log.info("Unable to start block synchronization in fast mode: pivot block is less than 1")
        appStateStorage.fastSyncDone().commit()
        context become idle
        syncController ! Done
      } else {
        val initialSyncState = generateInitialState(pivotBlockHeader)
        val syncingHandler = context.actorOf(syncingHandlerProps(initialSyncState))
        syncingHandler ! SyncingHandler.ProcessSyncing
        context become handlingSync(syncingHandler)
      }
  }

  private def generateInitialState(pivotBlockHeader: BlockHeader) = {
    val safeDownloadTarget = pivotBlockHeader.number + syncConfig.fastSyncBlockValidationX
    SyncState(pivotBlockHeader, safeDownloadTarget = safeDownloadTarget)
  }

  def handlingSync(syncingHandler: ActorRef): Receive = {
    case Done =>
      syncStateStorageActor ! PoisonPill
      fastSyncStateStorage.purge()
      syncController ! Done
      context become idle
    case msg =>
      syncingHandler.forward(msg)
  }
}

object FastSync {

  case class PeerWithInfo(peer: Peer, info: PeerInfo)

  // scalastyle:off parameter.number
  def props(
      fastSyncStateStorage: FastSyncStateStorage,
      appStateStorage: AppStateStorage,
      blockchain: Blockchain,
      validators: Validators,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new FastSync(
        fastSyncStateStorage,
        appStateStorage,
        blockchain,
        validators,
        peerEventBus,
        etcPeerManager,
        syncConfig,
        scheduler
      )
    )

  private[fast] case class UpdatePivotBlock(reason: PivotBlockUpdateReason)
  private[fast] case object ProcessSyncing

  private[sync] case object PersistSyncState

  private[fast] case object PrintStatus

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Done

  sealed abstract class HeaderProcessingResult
  case object HeadersProcessingFinished extends HeaderProcessingResult
  case class ParentChainWeightNotFound(header: BlockHeader) extends HeaderProcessingResult
  case class ValidationFailed(header: BlockHeader, peer: Peer) extends HeaderProcessingResult
  case object ImportedPivotBlock extends HeaderProcessingResult

  sealed abstract class PivotBlockUpdateReason {
    def isSyncRestart: Boolean = this match {
      case ImportedLastBlock => false
      case LastBlockValidationFailed => false
      case SyncRestart => true
    }
  }

  case object ImportedLastBlock extends PivotBlockUpdateReason
  case object LastBlockValidationFailed extends PivotBlockUpdateReason
  case object SyncRestart extends PivotBlockUpdateReason
}
