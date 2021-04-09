package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.Behavior
import akka.actor.Status.Failure
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.option._
import cats.syntax.either._
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
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

  implicit val ec: MonixScheduler = MonixScheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication
  private val log = context.log

  override def onMessage(message: FetchCommand): Behavior[FetchCommand] = {
    Behaviors.receive { (_, message) =>
      message match {
        case Start(fromBlock) => BlockFetcherState.initial(blockValidator, fromBlock) |> fetchBlocks
      }
    }
  }

  private def processBlockImporterCommands(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
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
          fetchStateNode(hash, replyTo, state)

        case BlockImportFailed(blockNr, reason) =>
          val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
          peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
          fetchBlocks(newState)
      }
    }


  private def processHeaders(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case Response(_, BlockHeaders(headers)) =>
          log.debug("Fetched {} headers starting from block {}", headers.size, headers.headOption.map(_.number))
          state.appendHeaders(headers) match {
            case Left(err) =>
              log.info("Dismissed received headers due to: {}", err)
              processBodies(state)
            case Right(updatedState) =>
              processBodies(updatedState)
          }
          //First successful fetch
//          if (state.waitingHeaders.isEmpty) {
//            supervisor ! ProgressProtocol.StartedFetching
//          }
        case RetryHeadersRequest =>
          log.debug("Something failed on a headers request, cancelling the request and re-fetching")
          processBodies(state)
      }
    }

  private def processBodies(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case Response(peer, BlockBodies(bodies)) =>
          log.debug(s"Received ${bodies.size} block bodies")
          val newState =
            state.validateBodies(bodies) match {
              case Left(err) =>
                peersClient ! BlacklistPeer(peer.id, err)
                state
              case Right(newBlocks) =>
                state.handleRequestedBlocks(newBlocks, peer.id)
            }
          val waitingHeadersDequeued = state.waitingHeaders.size - newState.waitingHeaders.size
          log.debug(s"Processed $waitingHeadersDequeued new blocks from received block bodies")
          processBlockImporterCommands(newState)
        case RetryBodiesRequest =>
          log.debug("Something failed on a bodies request, cancelling the request and re-fetching")
          processBlockImporterCommands(state)
      }
    }


  private def processStateNode(state: BlockFetcherState): Behavior[FetchCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case Response(peer, NodeData(values)) if state.isFetchingStateNode =>
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
                processBlockImporterCommands(state.notFetchingStateNode())
            }
          }).getOrElse(processBlockImporterCommands(state.notFetchingStateNode()))

        case RetryFetchStateNode if state.isFetchingStateNode =>
          state.stateNodeFetcher.collect(fetcher => fetchStateNode(fetcher.hash, fetcher.replyTo, state)).
            getOrElse(processBlockImporterCommands(state.notFetchingStateNode()))
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
    processHeaders(newState)
  }

  private def tryFetchHeaders(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(!_.hasReachedSize(syncConfig.maxFetcherQueueSize))
      .tap(fetchHeaders)
      .getOrElse(fetcherState)

  private def fetchHeaders(state: BlockFetcherState): Unit = {
    val blockNr = state.nextBlockToFetch
    val amount = syncConfig.blockHeadersPerRequest

    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    val resp = makeRequest(Request.create(msg, BestPeer), RetryHeadersRequest)
      .flatMap {
        case Response(_, BlockHeaders(headers)) if headers.isEmpty =>
          log.debug("Empty BlockHeaders response. Retry in {}", syncConfig.syncRetryInterval)
          Task.now(RetryHeadersRequest).delayResult(syncConfig.syncRetryInterval)
        case res => Task.now(res)
      }

    context.pipeToSelf(resp.runToFuture)(_)
  }

  private def tryFetchBodies(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .getOrElse(fetcherState)

  private def fetchBodies(state: BlockFetcherState): Unit = {
    log.debug("Fetching bodies")

    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)
    val resp = makeRequest(Request.create(GetBlockBodies(hashes), BestPeer), RetryBodiesRequest)
    context.pipeToSelf(resp.runToFuture)(_)
  }

  private def fetchStateNode(hash: ByteString, originalSender: ClassicActorRef, state: BlockFetcherState): Behavior[FetchCommand] = {
    log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
    val resp = makeRequest(Request.create(GetNodeData(List(hash)), BestPeer), RetryFetchStateNode)
    context.pipeToSelf(resp.runToFuture)(_)
    val newState = state.fetchingStateNode(hash, originalSender)
    processStateNode(newState)
  }

  private def makeRequest(request: Request[_], responseFallback: FetchCommand): Task[Any] =
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

  private def handleRequestResult(fallback: FetchCommand)(msg: Any): Task[Any] = msg match {
    case failed: RequestFailed =>
      log.debug("Request failed due to {}", failed)
      Task.now(fallback)
    case NoSuitablePeer =>
      Task.now(fallback).delayExecution(syncConfig.syncRetryInterval)
    case Failure(cause) =>
      log.error("Unexpected error on the request result", cause)
      Task.now(fallback)
    case m =>
      Task.now(m)
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
  final case class FetchStateNode(hash: ByteString, replyTo: ClassicActorRef) extends FetchCommand
  final case object RetryFetchStateNode extends FetchCommand
  final case class PickBlocks(amount: Int, replyTo: ClassicActorRef) extends FetchCommand
  final case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt, replyTo: ClassicActorRef) extends FetchCommand
  final case object PrintStatus extends FetchCommand
  final case object RetryBodiesRequest extends FetchCommand
  final case object RetryHeadersRequest extends FetchCommand
  final case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchCommand
  final case class InvalidateBlocksFrom(fromBlock: BigInt, reason: String, shouldBlacklist: Option[BigInt]) extends FetchCommand
  object InvalidateBlocksFrom {
    def apply(from: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, if (shouldBlacklist) Some(from) else None)
  }

  sealed trait FetchEvent
  final case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchEvent
  final case class FetchedStateNode(stateNode: NodeData) extends FetchEvent
}
