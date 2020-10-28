package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, AllForOneStrategy, Cancellable, Props, Scheduler, SupervisorStrategy}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlockBroadcast
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils}
import io.iohk.ethereum.utils.Config.SyncConfig

class RegularSync(
    peersClient: ActorRef,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {
  import RegularSync._

  val fetcher: ActorRef =
    context.actorOf(BlockFetcher.props(peersClient, peerEventBus, syncConfig, scheduler), "block-fetcher")
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
        blockchainConfig,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        checkpointBlockGenerator
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

  override def receive: Receive = {
    case Start =>
      log.info("Starting regular sync")
      importer ! BlockImporter.Start
    case MinedBlock(block) =>
      log.info(s"Block mined [number = {}, hash = {}]", block.number, block.header.hashAsHexString)
      importer ! BlockImporter.MinedBlock(block)

    case NewCheckpoint(parentHash, signatures) =>
      log.info(s"Received new checkpoint for block ${ByteStringUtils.hash2string(parentHash)}")
      importer ! BlockImporter.NewCheckpoint(parentHash, signatures)
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
      blockchainConfig: BlockchainConfig,
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
        blockchainConfig,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        checkpointBlockGenerator,
        scheduler
      )
    )

  sealed trait RegularSyncMsg
  case object Start extends RegularSyncMsg
  case class MinedBlock(block: Block) extends RegularSyncMsg
  case class NewCheckpoint(parentHash: ByteString, signatures: Seq[ECDSASignature]) extends RegularSyncMsg
}
