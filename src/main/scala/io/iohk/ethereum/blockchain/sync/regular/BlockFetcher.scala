package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.option._
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcherState.{
  AwaitingBodiesToBeIgnored,
  AwaitingHeadersToBeIgnored
}
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, NotOnTop, OnTop}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.{Peer, PeerEventBusActor, PeerId}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.{Codes, CommonMessages, PV164}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.execution.{Scheduler => MonixScheduler}
import mouse.all._
import akka.actor.typed.scaladsl.adapter._

import scala.concurrent.duration._

class BlockFetcher(
    val peersClient: ClassicActorRef,
    val peerEventBus: ClassicActorRef,
    val supervisor: ClassicActorRef,
    val syncConfig: SyncConfig,
    val blockValidator: BlockValidator,
    context: ActorContext[BlockFetcher.FetchCommand]
) extends AbstractBehavior[BlockFetcher.FetchCommand](context) {

  import BlockFetcher._

  implicit val ec: MonixScheduler = MonixScheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication
  private val log = context.log

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

  private def subscribeAdapter(
      fetcher: ActorRef[BlockFetcher.AdaptedMessageFromEventBus]
  ): Behaviors.Receive[MessageFromPeer] =
    Behaviors.receiveMessage[PeerEventBusActor.PeerEvent.MessageFromPeer] { case MessageFromPeer(message, peerId) =>
      fetcher ! AdaptedMessageFromEventBus(message, peerId)
      Behaviors.same
    }

  override def onMessage(message: FetchCommand): Behavior[FetchCommand] = {
    message match {
      case Start(importer, fromBlock) =>
        val sa = context.spawn(subscribeAdapter(context.self), "fetcher-subscribe-adapter")
        peerEventBus.tell(
          Subscribe(
            MessageClassifier(
              Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
              PeerSelector.AllPeers
            )
          ),
          sa.toClassic
        )
        BlockFetcherState.initial(importer, blockValidator, fromBlock) |> fetchBlocks
      case msg =>
        log.debug("Fetcher subscribe adapter received unhandled message {}", msg)
        Behaviors.unhandled
    }
  }

  // scalastyle:off cyclomatic.complexity method.length
  private def processFetchCommands(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receiveMessage {
      case PrintStatus =>
        log.info("{}", state.status)
        log.debug("{}", state.statusDetailed)
        Behaviors.same

      case PickBlocks(amount, replyTo) =>
        state.pickBlocks(amount) |> handlePickedBlocks(state, replyTo) |> fetchBlocks

      case StrictPickBlocks(from, atLeastWith, replyTo) =>
        // FIXME: Consider having StrictPickBlocks calls guaranteeing this
        // from parameter could be negative or 0 so we should cap it to 1 if that's the case
        val fromCapped = from.max(1)
        val minBlock = fromCapped.min(atLeastWith).max(1)
        log.debug("Strict Pick blocks from {} to {}", fromCapped, atLeastWith)
        log.debug("Lowest available block is {}", state.lowestBlock)

        val newState = if (minBlock < state.lowestBlock) {
          state.invalidateBlocksFrom(minBlock, None)._2
        } else {
          state.strictPickBlocks(fromCapped, atLeastWith) |> handlePickedBlocks(state, replyTo)
        }

        fetchBlocks(newState)

      case InvalidateBlocksFrom(blockNr, reason, withBlacklist) =>
        val (blockProvider, newState) = state.invalidateBlocksFrom(blockNr, withBlacklist)
        log.debug("Invalidate blocks from {}", blockNr)
        blockProvider.foreach(peersClient ! BlacklistPeer(_, BlacklistReason.BlockImportError(reason)))
        fetchBlocks(newState)

      case ReceivedHeaders(headers) if state.isFetchingHeaders =>
        //First successful fetch
        if (state.waitingHeaders.isEmpty) {
          supervisor ! ProgressProtocol.StartedFetching
        }
        val newState =
          if (state.fetchingHeadersState == AwaitingHeadersToBeIgnored) {
            log.debug(
              "Received {} headers starting from block {} that will be ignored",
              headers.size,
              headers.headOption.map(_.number)
            )
            state.withHeaderFetchReceived
          } else {
            log.debug("Fetched {} headers starting from block {}", headers.size, headers.headOption.map(_.number))
            state.appendHeaders(headers) match {
              case Left(err) =>
                log.info("Dismissed received headers due to: {}", err)
                state.withHeaderFetchReceived
              case Right(updatedState) =>
                updatedState.withHeaderFetchReceived
            }
          }
        fetchBlocks(newState)
      case RetryHeadersRequest if state.isFetchingHeaders =>
        log.debug("Something failed on a headers request, cancelling the request and re-fetching")
        fetchBlocks(state.withHeaderFetchReceived)

      case ReceivedBodies(peer, bodies) if state.isFetchingBodies =>
        log.debug("Received {} block bodies", bodies.size)
        if (state.fetchingBodiesState == AwaitingBodiesToBeIgnored) {
          log.debug("Block bodies will be ignored due to an invalidation was requested for them")
          fetchBlocks(state.withBodiesFetchReceived)
        } else {
          val newState =
            state.validateBodies(bodies) match {
              case Left(err) =>
                peersClient ! BlacklistPeer(peer.id, err)
                state.withBodiesFetchReceived
              case Right(newBlocks) =>
                state.withBodiesFetchReceived.handleRequestedBlocks(newBlocks, peer.id)
            }
          val waitingHeadersDequeued = state.waitingHeaders.size - newState.waitingHeaders.size
          log.debug("Processed {} new blocks from received block bodies", waitingHeadersDequeued)
          fetchBlocks(newState)
        }

      case RetryBodiesRequest if state.isFetchingBodies =>
        log.debug("Something failed on a bodies request, cancelling the request and re-fetching")
        fetchBlocks(state.withBodiesFetchReceived)

      case FetchStateNode(hash, replyTo) =>
        log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
        stateNodeFetcher ! StateNodeFetcher.FetchStateNode(hash, replyTo)
        Behaviors.same

      case AdaptedMessageFromEventBus(NewBlockHashes(hashes), _) =>
        log.debug("Received NewBlockHashes numbers {}", hashes.map(_.number).mkString(", "))
        val newState = state.validateNewBlockHashes(hashes) match {
          case Left(_) => state
          case Right(validHashes) => state.withPossibleNewTopAt(validHashes.lastOption.map(_.number))
        }
        supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
        fetchBlocks(newState)

      case AdaptedMessageFromEventBus(CommonMessages.NewBlock(block, _), peerId) =>
        handleNewBlock(block, peerId, state)

      case AdaptedMessageFromEventBus(PV164.NewBlock(block, _), peerId) =>
        handleNewBlock(block, peerId, state)

      case BlockImportFailed(blockNr, reason) =>
        val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
        peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
        fetchBlocks(newState)

      case AdaptedMessageFromEventBus(BlockHeaders(headers), _) =>
        headers.lastOption
          .map { bh =>
            log.debug("Candidate for new top at block {}, current known top {}", bh.number, state.knownTop)
            val newState = state.withPossibleNewTopAt(bh.number)
            fetchBlocks(newState)
          }
          .getOrElse(processFetchCommands(state))
      //keep fetcher state updated in case new mined block was imported
      case InternalLastBlockImport(blockNr) =>
        log.debug("New mined block {} imported from the inside", blockNr)
        val newState = state.withLastBlock(blockNr).withPossibleNewTopAt(blockNr)
        fetchBlocks(newState)

      case msg =>
        log.debug("Block fetcher received unhandled message {}", msg)
        Behaviors.unhandled
    }

  private def handleNewBlock(block: Block, peerId: PeerId, state: BlockFetcherState): Behavior[FetchCommand] = {
    log.debug("Received NewBlock {}", block.idTag)
    val newBlockNr = block.number
    val nextExpectedBlock = state.lastBlock + 1

    if (state.isOnTop && newBlockNr == nextExpectedBlock) {
      log.debug("Passing block directly to importer")
      val newState = state
        .withPeerForBlocks(peerId, Seq(newBlockNr))
        .withLastBlock(newBlockNr)
        .withKnownTopAt(newBlockNr)
      state.importer ! OnTop
      state.importer ! ImportNewBlock(block, peerId)
      supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
      processFetchCommands(newState)
    } else {
      handleFutureBlock(block, state)
    }
  }

  private def handleFutureBlock(block: Block, state: BlockFetcherState): Behavior[FetchCommand] = {
    log.debug("Ignoring received block as it doesn't match local state or fetch side is not on top")
    val newState = state.withPossibleNewTopAt(block.number)
    supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
    fetchBlocks(newState)
  }

  private def handlePickedBlocks(
      state: BlockFetcherState,
      replyTo: ClassicActorRef
  )(pickResult: Option[(NonEmptyList[Block], BlockFetcherState)]): BlockFetcherState =
    pickResult
      .tap { case (blocks, newState) =>
        replyTo ! PickedBlocks(blocks)
        replyTo ! (if (newState.isOnTop) OnTop else NotOnTop)
      }
      .fold(state)(_._2)

  private def fetchBlocks(state: BlockFetcherState): Behavior[FetchCommand] = {
    val newState = state |> tryFetchHeaders |> tryFetchBodies
    processFetchCommands(newState)
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
  def apply(
      peersClient: ClassicActorRef,
      peerEventBus: ClassicActorRef,
      supervisor: ClassicActorRef,
      syncConfig: SyncConfig,
      blockValidator: BlockValidator
  ): Behavior[FetchCommand] =
    Behaviors.setup(context =>
      new BlockFetcher(peersClient, peerEventBus, supervisor, syncConfig, blockValidator, context)
    )

  sealed trait FetchCommand
  final case class Start(importer: ClassicActorRef, fromBlock: BigInt) extends FetchCommand
  final case class FetchStateNode(hash: ByteString, replyTo: ClassicActorRef) extends FetchCommand
  final case object RetryFetchStateNode extends FetchCommand
  final case class PickBlocks(amount: Int, replyTo: ClassicActorRef) extends FetchCommand
  final case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt, replyTo: ClassicActorRef) extends FetchCommand
  final case object PrintStatus extends FetchCommand
  final case class InvalidateBlocksFrom(fromBlock: BigInt, reason: String, toBlacklist: Option[BigInt])
      extends FetchCommand

  object InvalidateBlocksFrom {

    def apply(from: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, if (shouldBlacklist) Some(from) else None)

    def apply(from: BigInt, reason: String, toBlacklist: Option[BigInt]): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, toBlacklist)
  }
  final case class BlockImportFailed(blockNr: BigInt, reason: BlacklistReason) extends FetchCommand
  final case class InternalLastBlockImport(blockNr: BigInt) extends FetchCommand
  final case object RetryBodiesRequest extends FetchCommand
  final case object RetryHeadersRequest extends FetchCommand
  final case class AdaptedMessageFromEventBus(message: Message, peerId: PeerId) extends FetchCommand
  final case class ReceivedHeaders(headers: Seq[BlockHeader]) extends FetchCommand
  final case class ReceivedBodies(peer: Peer, bodies: Seq[BlockBody]) extends FetchCommand

  sealed trait FetchResponse
  final case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchResponse
  final case class FetchedStateNode(stateNode: NodeData) extends FetchResponse
}
