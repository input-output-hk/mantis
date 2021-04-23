package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.option._
import akka.pattern.ask
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcherState.{AwaitingBodiesToBeIgnored, AwaitingHeadersToBeIgnored}
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, NotOnTop, OnTop}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.{Codes, CommonMessages, PV64}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.execution.{Scheduler => MonixScheduler}
import mouse.all._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

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

  override def onMessage(message: FetchCommand): Behavior[FetchCommand] = {
    message match {
      case Start(importer, fromBlock) =>
        peerEventBus ! Subscribe(
          MessageClassifier(
            Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
            PeerSelector.AllPeers
          ))
        BlockFetcherState.initial(importer, blockValidator, fromBlock) |> fetchBlocks
      case _ => Behaviors.unhandled
    }
  }

  private def processFetchCommands(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case PrintStatus =>
          log.info("{}", state.status)
          log.debug("{}", state.statusDetailed)
          Behaviors.same

        case PickBlocks(amount, replyTo) => state.pickBlocks(amount) |> handlePickedBlocks(state, replyTo) |> fetchBlocks

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
          blockProvider.foreach(peersClient ! BlacklistPeer(_, reason))
          fetchBlocks(newState)

        case AdaptedMessage(_, BlockHeaders(headers)) if state.isFetchingHeaders =>
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

          //First successful fetch
          if (state.waitingHeaders.isEmpty) {
            supervisor ! ProgressProtocol.StartedFetching
          }

          fetchBlocks(newState)
        case RetryHeadersRequest if state.isFetchingHeaders =>
          log.debug("Something failed on a headers request, cancelling the request and re-fetching")

          val newState = state.withHeaderFetchReceived
          fetchBlocks(newState)

        case AdaptedMessage(peer, BlockBodies(bodies)) if state.isFetchingBodies =>
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
          val newState = state.withBodiesFetchReceived
          fetchBlocks(newState)

        case FetchStateNode(hash, replyTo) => fetchStateNode(hash, replyTo, state)

        case RetryFetchStateNode if state.isFetchingStateNode =>
          state.stateNodeFetcher.collect(fetcher => fetchStateNode(fetcher.hash, fetcher.replyTo, state)).
            getOrElse(processFetchCommands(state.notFetchingStateNode()))

        case AdaptedMessage(peer, NodeData(values)) if state.isFetchingStateNode =>
          log.debug("Received state node response from peer {}", peer)
          state.stateNodeFetcher.collect(fetcher => {
            val validatedNode = values
              .asRight[String]
              .ensure(s"Empty response from peer $peer, blacklisting")(_.nonEmpty)
              .ensure("Fetched node state hash doesn't match requested one, blacklisting peer")(nodes =>
                fetcher.hash == kec256(nodes.head)
              )

            validatedNode match {
              case Left(err) =>
                log.debug(err)
                peersClient ! BlacklistPeer(peer.id, err)
                fetchStateNode(fetcher.hash, fetcher.replyTo, state)
              case Right(node) =>
                fetcher.replyTo ! FetchedStateNode(NodeData(node))
                processFetchCommands(state.notFetchingStateNode())
            }
          }).getOrElse(processFetchCommands(state.notFetchingStateNode()))


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

        case AdaptedMessageFromEventBus(PV64.NewBlock(block, _), peerId) =>
          handleNewBlock(block, peerId, state)

        case BlockImportFailed(blockNr, reason) =>
          val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
          peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
          fetchBlocks(newState)

        case AdaptedMessageFromEventBus(BlockHeaders(headers), _) =>
          headers.lastOption.map { bh =>
            log.debug("Candidate for new top at block {}, current known top {}", bh.number, state.knownTop)
            val newState = state.withPossibleNewTopAt(bh.number)
            fetchBlocks(newState)
          }.getOrElse(processFetchCommands(state))
        //keep fetcher state updated in case new mined block was imported
        case InternalLastBlockImport(blockNr) =>
          log.debug("New mined block {} imported from the inside", blockNr)
          val newState = state.withLastBlock(blockNr).withPossibleNewTopAt(blockNr)

          fetchBlocks(newState)

        case _ =>
          Behaviors.unhandled
      }
    }

  private def handleNewBlock(block: Block, peerId: PeerId, state: BlockFetcherState): Behavior[FetchCommand] = {
    //TODO ETCM-389: Handle mined, checkpoint and new blocks uniformly
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
      processFetchCommands(state)
    }
  }

  private def handleFutureBlock(block: Block, state: BlockFetcherState): Unit = {
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
        //step2: replace newState.importer to replyTo
        newState.importer ! (if (newState.isOnTop) OnTop else NotOnTop)
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

    context.pipeToSelf(fetchHeadersFrom(blockNr, amount).runToFuture) {
      case Success(res) => res
      case Failure(_) => RetryHeadersRequest
    }
  }

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Task[FetchCommand] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    requestBlockHeaders(msg)
  }

  private def tryFetchBodies(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .map(_.withNewBodiesFetch)
      .getOrElse(fetcherState)

  private def fetchBodies(state: BlockFetcherState): Unit = {
    log.debug("Fetching bodies")

    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)

    context.pipeToSelf(requestBlockBodies(hashes).runToFuture) {
      case Success(res) => res
      case Failure(_) => RetryBodiesRequest
    }
  }

  private def fetchStateNode(hash: ByteString, originalSender: ClassicActorRef, state: BlockFetcherState): Behavior[FetchCommand] = {
    log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
    context.pipeToSelf(requestStateNode(hash).runToFuture) {
      case Success(res) => res
      case Failure(_) => RetryFetchStateNode
    }
    val newState = state.fetchingStateNode(hash, originalSender)

    processFetchCommands(newState)
  }

  private def requestBlockHeaders(msg: GetBlockHeaders): Task[FetchCommand] =
  makeRequest(Request.create(msg, BestPeer), RetryHeadersRequest)
      .flatMap {
        case AdaptedMessage(_, BlockHeaders(headers)) if headers.isEmpty =>
          log.debug("Empty BlockHeaders response. Retry in {}", syncConfig.syncRetryInterval)
          Task.now(RetryHeadersRequest).delayResult(syncConfig.syncRetryInterval)
        case res => Task.now(res)
      }

  private def requestBlockBodies(hashes: Seq[ByteString]): Task[FetchCommand] =
    makeRequest(Request.create(GetBlockBodies(hashes), BestPeer), RetryBodiesRequest)

  private def requestStateNode(hash: ByteString): Task[FetchCommand] =
    makeRequest(Request.create(GetNodeData(List(hash)), BestPeer), RetryFetchStateNode)

  private def makeRequest(request: Request[_], responseFallback: FetchCommand): Task[FetchCommand] =
    Task
      .deferFuture(peersClient ? request)
      .tap(blacklistPeerOnFailedRequest)
      .flatMap(handleRequestResult(responseFallback))
      .onErrorHandle { error =>
        log.error("Unexpected error while doing a request", error)
        responseFallback
      }

  private def blacklistPeerOnFailedRequest(msg: Any): Unit = msg match {
    case RequestFailed(peer, reason) => peersClient ! BlacklistPeer(peer.id, reason)
    case _ => ()
  }

  private def handleRequestResult(fallback: FetchCommand)(msg: Any): Task[FetchCommand] = msg match {
    case failed: RequestFailed =>
      log.warn("Request failed due to {}", failed)
      Task.now(fallback)

    case NoSuitablePeer =>
      Task.now(fallback).delayExecution(syncConfig.syncRetryInterval)

    case Failure(cause) =>
      log.error("Unexpected error on the request result", cause)
      Task.now(fallback)

    case PeersClient.Response(peer, msg) =>
      Task.now(AdaptedMessage(peer, msg))
  }
}

object BlockFetcher {
  def apply(peersClient: ClassicActorRef,
            peerEventBus: ClassicActorRef,
            supervisor: ClassicActorRef,
            syncConfig: SyncConfig,
            blockValidator: BlockValidator): Behavior[FetchCommand] =
    Behaviors.setup(context => new BlockFetcher(peersClient, peerEventBus, supervisor, syncConfig, blockValidator, context))

  sealed trait FetchCommand
  final case class Start(importer: ClassicActorRef, fromBlock: BigInt) extends FetchCommand
  final case class FetchStateNode(hash: ByteString, replyTo: ClassicActorRef) extends FetchCommand
  final case object RetryFetchStateNode extends FetchCommand
  final case class PickBlocks(amount: Int, replyTo: ClassicActorRef) extends FetchCommand
  final case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt, replyTo: ClassicActorRef) extends FetchCommand
  final case object PrintStatus extends FetchCommand
  final case class InvalidateBlocksFrom(fromBlock: BigInt, reason: String, toBlacklist: Option[BigInt]) extends FetchCommand

  object InvalidateBlocksFrom {

    def apply(from: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, if (shouldBlacklist) Some(from) else None)

    def apply(from: BigInt, reason: String, toBlacklist: Option[BigInt]): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, toBlacklist)
  }
  final case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchCommand
  final case class InternalLastBlockImport(blockNr: BigInt) extends FetchCommand
  final case object RetryBodiesRequest extends FetchCommand
  final case object RetryHeadersRequest extends FetchCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends FetchCommand
  final case class AdaptedMessageFromEventBus(message: Message, peerId: PeerId) extends FetchCommand

  sealed trait FetchResponse
  final case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchResponse
  final case class FetchedStateNode(stateNode: NodeData) extends FetchResponse
}
