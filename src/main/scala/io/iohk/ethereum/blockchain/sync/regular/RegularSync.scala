package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.AllForOneStrategy
import akka.actor.Cancellable
import akka.actor.Props
import akka.actor.Scheduler
import akka.actor.SupervisorStrategy
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef => TypedActorRef}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.util.ByteString

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext

import io.iohk.ethereum.blockchain.sync.Blacklist
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.InternalLastBlockImport
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressState
import io.iohk.ethereum.consensus.ConsensusAdapter
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.branch.BestBranch
import io.iohk.ethereum.domain.branch.Branch
import io.iohk.ethereum.ledger.BranchResolution
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.PeerEventBusActor
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH62
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig

class RegularSync(
    peersClient: ActorRef,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    consensus: ConsensusAdapter,
    blockchainReader: BlockchainReader,
    stateStorage: StateStorage,
    branchResolution: BranchResolution,
    blockValidator: BlockValidator,
    blacklist: Blacklist,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    scheduler: Scheduler,
    configBuilder: BlockchainConfigBuilder
) extends Actor
    with ActorLogging {

  val fetcher: TypedActorRef[BlockFetcher.FetchCommand] =
    context.spawn(
      BlockFetcher(peersClient, peerEventBus, self, syncConfig, blockValidator, disableBlockProcessing = true),
      "block-fetcher"
    )

  context.watch(fetcher)

  val broadcaster: ActorRef = context.actorOf(
    BlockBroadcasterActor
      .props(new BlockBroadcast(etcPeerManager), peerEventBus, etcPeerManager, blacklist, syncConfig, scheduler),
    "block-broadcaster"
  )
  val importer: ActorRef =
    context.actorOf(
      BlockImporter.props(
        fetcher.toClassic,
        consensus,
        blockchainReader,
        stateStorage,
        branchResolution,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        self,
        configBuilder
      ),
      "block-importer"
    )

  implicit val system: ActorSystem = context.system
  implicit val ec: ExecutionContext = context.dispatcher

  val printFetcherSchedule: Cancellable =
    scheduler.scheduleWithFixedDelay(
      syncConfig.printStatusInterval,
      syncConfig.printStatusInterval,
      fetcher.toClassic,
      BlockFetcher.PrintStatus
    )

  PeerEventBusActor
    .messageSource(
      peerEventBus,
      PeerEventBusActor.SubscriptionClassifier
        .MessageClassifier(
          Set(Codes.BlockBodiesCode, Codes.BlockHeadersCode),
          PeerEventBusActor.PeerSelector.AllPeers
        )
    )
    .buffer(10000, OverflowStrategy.fail)
    .via(
      FetcherService.fetchBlocksForHeaders(
        Sink.ignore // BlockFetcher is relied on for requesting the bodies
      )
    )
    .via(BranchBuffer.flow(blockchainReader))
    .runWith(Sink.foreach { blocks =>
      importer ! BlockFetcher.PickedBlocks(blocks)
    })
    .onComplete(println(_))

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

    case NewCheckpoint(block) =>
      log.info(s"Received new checkpoint for block ${ByteStringUtils.hash2string(block.header.parentHash)}")
      importer ! BlockImporter.NewCheckpoint(block)

    case SyncProtocol.GetStatus =>
      sender() ! progressState.toStatus

    case ProgressProtocol.StartedFetching =>
      val newState = progressState.copy(startedFetching = true)
      context.become(running(newState))
    case ProgressProtocol.StartingFrom(blockNumber) =>
      val newState = progressState.copy(initialBlock = blockNumber, currentBlock = blockNumber)
      context.become(running(newState))
    case ProgressProtocol.GotNewBlock(blockNumber) =>
      log.info(s"Got information about new block [number = $blockNumber]")
      val newState = progressState.copy(bestKnownNetworkBlock = blockNumber)
      context.become(running(newState))
    case ProgressProtocol.ImportedBlock(blockNumber, internally) =>
      log.info(s"Imported new block [number = $blockNumber, internally = $internally]")
      val newState = progressState.copy(currentBlock = blockNumber)
      if (internally) {
        fetcher ! InternalLastBlockImport(blockNumber)
      }
      context.become(running(newState))
  }

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy()(SupervisorStrategy.defaultDecider)

  override def postStop(): Unit = {
    log.info("Regular Sync stopped")
    printFetcherSchedule.cancel()
  }
}
object RegularSync {
  // scalastyle:off parameter.number
  def props(
      peersClient: ActorRef,
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      consensus: ConsensusAdapter,
      blockchainReader: BlockchainReader,
      stateStorage: StateStorage,
      branchResolution: BranchResolution,
      blockValidator: BlockValidator,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      pendingTransactionsManager: ActorRef,
      scheduler: Scheduler,
      configBuilder: BlockchainConfigBuilder
  ): Props =
    Props(
      new RegularSync(
        peersClient,
        etcPeerManager,
        peerEventBus,
        consensus,
        blockchainReader,
        stateStorage,
        branchResolution,
        blockValidator,
        blacklist,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        scheduler,
        configBuilder
      )
    )

  case class NewCheckpoint(block: Block)

  case class ProgressState(
      startedFetching: Boolean,
      initialBlock: BigInt,
      currentBlock: BigInt,
      bestKnownNetworkBlock: BigInt
  ) {
    def toStatus: SyncProtocol.Status =
      if (startedFetching && bestKnownNetworkBlock != 0 && currentBlock < bestKnownNetworkBlock) {
        Status.Syncing(initialBlock, Progress(currentBlock, bestKnownNetworkBlock), None)
      } else if (startedFetching && currentBlock >= bestKnownNetworkBlock) {
        Status.SyncDone
      } else {
        Status.NotSyncing
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
