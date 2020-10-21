package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, AllForOneStrategy, Cancellable, Props, Scheduler, SupervisorStrategy}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.{BlockBroadcast, SyncProtocol}
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.InternalLastBlockImport
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.{NewCheckpoint, ProgressProtocol, ProgressState}
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig

class RegularSync(
    peersClient: ActorRef,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {

  val fetcher: ActorRef =
    context.actorOf(BlockFetcher.props(peersClient, peerEventBus, self, syncConfig, scheduler), "block-fetcher")
  val broadcaster: ActorRef = context.actorOf(
    BlockBroadcasterActor
      .props(new BlockBroadcast(etcPeerManager, syncConfig), peerEventBus, etcPeerManager, syncConfig, scheduler),
    "block-broadcaster"
  )
  val importer: ActorRef =
    context.actorOf(
      BlockImporter.props(
        fetcher,
        ledger,
        blockchain,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        checkpointBlockGenerator,
        self
      ),
      "block-importer"
    )

  val printFetcherSchedule: Cancellable =
    scheduler.scheduleWithFixedDelay(
      syncConfig.printStatusInterval,
      syncConfig.printStatusInterval,
      fetcher,
      BlockFetcher.PrintStatus
    )(context.dispatcher)
  val printImporterSchedule: Cancellable =
    scheduler.scheduleWithFixedDelay(
      syncConfig.printStatusInterval,
      syncConfig.printStatusInterval,
      importer,
      BlockImporter.PrintStatus
    )(context.dispatcher)

  override def receive: Receive = running(
    ProgressState(startedFetching = false, initialBlock = 0, currentBlock = 0, bestKnownNetworkBlock = 0)
  )

  def running(progressState: ProgressState): Receive = {
    case SyncProtocol.Start =>
      log.info("Starting regular sync")
      importer ! BlockImporter.Start
    case SyncProtocol.MinedBlock(block) =>
      log.info(s"Block mined [number = {}, hash = {}]", block.number, block.header.hashAsHexString)
      importer ! BlockImporter.MinedBlock(block)

    case NewCheckpoint(parentHash, signatures) =>
      log.info(s"Received new checkpoint for block ${ByteStringUtils.hash2string(parentHash)}")
      importer ! BlockImporter.NewCheckpoint(parentHash, signatures)

    case SyncProtocol.GetStatus =>
      sender() ! progressState.toStatus

    case ProgressProtocol.StartedFetching =>
      val newState = progressState.copy(startedFetching = true)
      context become running(newState)
    case ProgressProtocol.StartingFrom(blockNumber) =>
      val newState = progressState.copy(initialBlock = blockNumber, currentBlock = blockNumber)
      context become running(newState)
    case ProgressProtocol.GotNewBlock(blockNumber) =>
      val newState = progressState.copy(bestKnownNetworkBlock = blockNumber)
      context become running(newState)
    case ProgressProtocol.ImportedBlock(blockNumber, internally) =>
      val newState = progressState.copy(currentBlock = blockNumber)
      if (internally) {
        fetcher ! InternalLastBlockImport(blockNumber)
      }
      context become running(newState)
  }

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy()(SupervisorStrategy.defaultDecider)

  override def postStop(): Unit = {
    log.info("Regular Sync stopped")
    printFetcherSchedule.cancel()
    printImporterSchedule.cancel()
  }
}
object RegularSync {
  // scalastyle:off parameter.number
  def props(
      peersClient: ActorRef,
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      pendingTransactionsManager: ActorRef,
      checkpointBlockGenerator: CheckpointBlockGenerator,
      scheduler: Scheduler
  ): Props =
    Props(
      new RegularSync(
        peersClient,
        etcPeerManager,
        peerEventBus,
        ledger,
        blockchain,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        checkpointBlockGenerator,
        scheduler
      )
    )

  case class NewCheckpoint(parentHash: ByteString, signatures: Seq[ECDSASignature])

  case class ProgressState(
      startedFetching: Boolean,
      initialBlock: BigInt,
      currentBlock: BigInt,
      bestKnownNetworkBlock: BigInt
  ) {
    def toStatus: SyncProtocol.Status = {
      if (startedFetching && bestKnownNetworkBlock != 0 && currentBlock < bestKnownNetworkBlock) {
        Status.Syncing(initialBlock, Progress(currentBlock, bestKnownNetworkBlock), None)
      } else if (startedFetching && currentBlock >= bestKnownNetworkBlock) {
        Status.SyncDone
      } else {
        Status.NotSyncing
      }
    }
  }
  sealed trait ProgressProtocol
  object ProgressProtocol {
    case object StartedFetching extends ProgressProtocol
    case class StartingFrom(blockNumber: BigInt) extends ProgressProtocol
    case class GotNewBlock(blockNumber: BigInt) extends ProgressProtocol
    case class ImportedBlock(blockNumber: BigInt, internally: Boolean) extends ProgressProtocol
  }
}
