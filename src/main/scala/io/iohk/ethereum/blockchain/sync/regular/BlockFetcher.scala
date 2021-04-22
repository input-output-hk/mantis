package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.option._
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.RetryHeadersRequest
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.execution.{Scheduler => MonixScheduler}
import mouse.all._

import scala.concurrent.duration._

class BlockFetcher(
    val peersClient: ClassicActorRef,
    val supervisor: ClassicActorRef,
    val syncConfig: SyncConfig,
    val blockValidator: BlockValidator,
    context: ActorContext[BlockFetcher.FetchCommand]
) extends AbstractBehavior[BlockFetcher.FetchCommand](context) {

  import BlockFetcher._

  val headersFetcher: ActorRef[HeadersFetcher.HeadersFetcherCommand] =
    context.spawn(
      HeadersFetcher(peersClient, syncConfig, context.self),
      "headers-fetcher"
    )
  context.watch(headersFetcher)

  val bodiesFetcher: ActorRef[BodiesFetcher.BodiesFetcherCommand] =
    context.spawn(
      BodiesFetcher(peersClient, syncConfig, context.self),
      "bodies-fetcher"
    )
  context.watch(bodiesFetcher)

  val stateNodeFetcher: ActorRef[StateNodeFetcher.StateNodeFetcherCommand] =
    context.spawn(
      StateNodeFetcher(peersClient, syncConfig, context.self),
      "state-node-fetcher"
    )
  context.watch(stateNodeFetcher)

  implicit val ec: MonixScheduler = MonixScheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication
  private val log = context.log

  override def onMessage(message: FetchCommand): Behavior[FetchCommand] = {
    message match {
      case Start(fromBlock) => BlockFetcherState.initial(blockValidator, fromBlock) |> fetchBlocks
      case _ => Behaviors.unhandled
    }
  }

  private def processBlockImporterCommands(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case PrintStatus =>
          log.info("{}", state.status)
          log.debug("{}", state.statusDetailed)
          Behaviors.same

        case UpdateKnownTop(blockNr) => state.withKnownTopAt(blockNr).withLastBlock(blockNr) |> fetchBlocks

        case LastImportedBlock(blockNr) =>
          log.debug("Last imported block number {}", blockNr)
          state.withLastBlock(blockNr).withUpdatedReadyBlocks(blockNr) |> fetchBlocks

        case PickBlocks(amount, replyTo) =>
          state.pickBlocks(amount) |> handlePickedBlocks(state, replyTo) |> fetchBlocks

        case StrictPickBlocks(from, atLeastWith, replyTo) =>
          // FIXME: Consider having StrictPickBlocks calls guaranteeing this
          // from parameter could be negative or 0 so we should cap it to 1 if that's the case
          val fromCapped = from.max(1)
          val minBlock = fromCapped.min(atLeastWith).max(1)
          log.debug("Strict Pick blocks from {} to {}", fromCapped, atLeastWith)
          log.debug("Lowest available block is {}", state.lowestBlock)

          val newState: BlockFetcherState = if (minBlock < state.lowestBlock) {
            state.invalidateBlocksFrom(minBlock, None)._2
          } else {
            state.strictPickBlocks(fromCapped, atLeastWith) |> handlePickedBlocks(state, replyTo)
          }

          fetchBlocks(newState)

        case InvalidateBlocksFrom(blockNr, reason, withBlacklist) =>
          val (blockProvider, newState) = state.invalidateBlocksFrom(blockNr, withBlacklist)
          log.debug("Invalidate blocks from {}", blockNr)
          blockProvider.foreach(peersClient ! BlacklistPeer(_, reason))
          fetchBlocks(newState)

        case FetchStateNode(hash, replyTo) =>
          log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
          stateNodeFetcher ! StateNodeFetcher.FetchStateNode(hash, replyTo)
          Behaviors.same

        case BlockImportFailed(blockNr, reason) =>
          val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
          peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
          fetchBlocks(newState)

        case ReceivedHeaders(headers) =>
          if (state.waitingHeaders.isEmpty) {
            supervisor ! ProgressProtocol.StartedFetching
          }
          state.appendHeaders(headers) match {
            case Left(err) =>
              log.info("Dismissed received headers due to: {}", err)
              fetchBlocks(state.withHeaderFetchReceived)
            case Right(updatedState) =>
              fetchBlocks(updatedState.withHeaderFetchReceived)
          }

        case RetryHeadersRequest if state.isFetchingHeaders =>
          log.debug("Something failed on a headers request, cancelling the request and re-fetching")
          fetchBlocks(state.withHeaderFetchReceived)

        case ReceivedBodies(peer, bodies) =>
          val newState =
            state.validateBodies(bodies) match {
              case Left(err) =>
                peersClient ! BlacklistPeer(peer.id, err)
                state.withBodiesFetchReceived
              case Right(newBlocks) =>
                state.withBodiesFetchReceived.handleRequestedBlocks(newBlocks, peer.id)
            }
          val waitingHeadersDequeued = state.waitingHeaders.size - newState.waitingHeaders.size
          log.debug(s"Processed $waitingHeadersDequeued new blocks from received block bodies")
          fetchBlocks(newState)

        case RetryBodiesRequest if state.isFetchingBodies =>
          log.debug("Something failed on a bodies request, cancelling the request and re-fetching")
          val newState = state.withBodiesFetchReceived
          fetchBlocks(newState)

        case _ =>
          Behaviors.unhandled
      }
    }

  private def handlePickedBlocks(
      state: BlockFetcherState,
      replyTo: ClassicActorRef
  )(pickResult: Option[(NonEmptyList[Block], BlockFetcherState)]): BlockFetcherState =
    pickResult
      .tap { case (blocks, _) =>
        replyTo ! PickedBlocks(blocks)
      }
      .fold(state)(_._2)

  private def fetchBlocks(state: BlockFetcherState): Behavior[FetchCommand] = {
    val newState = state |> tryFetchHeaders |> tryFetchBodies
    processBlockImporterCommands(newState)
  }

  private def tryFetchHeaders(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingHeaders)
      .filter(!_.hasFetchedTopHeader)
      .filter(!_.hasReachedSize(syncConfig.maxFetcherQueueSize))
      .tap(fetchHeaders)
      .map(_.withNewHeadersFetch)
      .getOrElse(fetcherState)

  private def fetchHeaders(state: BlockFetcherState): Unit = {
    val blockNr = state.nextBlockToFetch
    val amount = syncConfig.blockHeadersPerRequest
    headersFetcher ! HeadersFetcher.FetchHeaders(blockNr, amount)
  }

  private def tryFetchBodies(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .map(_.withNewBodiesFetch)
      .getOrElse(fetcherState)

  private def fetchBodies(state: BlockFetcherState): Unit = {
    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)
    bodiesFetcher ! BodiesFetcher.FetchBodies(hashes)
  }
}

object BlockFetcher {
  def apply(peersClient: ClassicActorRef,
            supervisor: ClassicActorRef,
            syncConfig: SyncConfig,
            blockValidator: BlockValidator): Behavior[FetchCommand] =
    Behaviors.setup(context => new BlockFetcher(peersClient, supervisor, syncConfig, blockValidator, context))

  sealed trait FetchCommand
  final case class Start(fromBlock: BigInt) extends FetchCommand
  final case class LastImportedBlock(blockNr: BigInt) extends FetchCommand
  final case class UpdateKnownTop(blockNr: BigInt) extends FetchCommand
  final case class FetchStateNode(hash: ByteString, replyTo: ClassicActorRef) extends FetchCommand
  final case class PickBlocks(amount: Int, replyTo: ClassicActorRef) extends FetchCommand
  final case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt, replyTo: ClassicActorRef) extends FetchCommand
  final case object PrintStatus extends FetchCommand
  final case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchCommand
  final case class InvalidateBlocksFrom(fromBlock: BigInt, reason: String, shouldBlacklist: Option[BigInt]) extends FetchCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends FetchCommand
  final case class ReceivedHeaders(headers: Seq[BlockHeader]) extends FetchCommand
  final case class ReceivedBodies(peer: Peer, bodies: Seq[BlockBody]) extends FetchCommand
  final case object RetryBodiesRequest extends FetchCommand
  final case object RetryHeadersRequest extends FetchCommand

  object InvalidateBlocksFrom {
    def apply(from: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, if (shouldBlacklist) Some(from) else None)
  }

  sealed trait FetchEvent
  final case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchEvent
  final case class FetchedStateNode(stateNode: NodeData) extends FetchEvent
}
