package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Status.Failure
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import akka.pattern.{ask, pipe}
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.future._
import cats.instances.option._
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcherState.{
  AwaitingBodiesToBeIgnored,
  AwaitingHeadersToBeIgnored
}
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, NotOnTop, OnTop}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import io.iohk.ethereum.utils.FutureOps._
import mouse.all._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class BlockFetcher(
    val peersClient: ActorRef,
    val peerEventBus: ActorRef,
    val supervisor: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging {

  import BlockFetcher._

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 1.second // some margin for actor communication

  override def receive: Receive = idle()

  override def postStop(): Unit = {
    super.postStop()
    peerEventBus ! Unsubscribe()
  }

  private def idle(): Receive = handleCommonMessages(None) orElse { case Start(importer, blockNr) =>
    BlockFetcherState.initial(importer, blockNr) |> fetchBlocks
    peerEventBus ! Subscribe(
      MessageClassifier(
        Set(NewBlock.code63, NewBlock.code64, NewBlockHashes.code, BlockHeaders.code),
        PeerSelector.AllPeers
      )
    )
  }

  def handleCommonMessages(state: Option[BlockFetcherState]): Receive = { case PrintStatus =>
    log.info("{}", state.map(_.status))
    log.debug("{}", state.map(_.statusDetailed))
  }

  private def started(state: BlockFetcherState): Receive =
    handleCommonMessages(Some(state)) orElse
      handleCommands(state) orElse
      handleNewBlockMessages(state) orElse
      handleHeadersMessages(state) orElse
      handleBodiesMessages(state) orElse
      handleStateNodeMessages(state) orElse
      handlePossibleTopUpdate(state)

  private def handleCommands(state: BlockFetcherState): Receive = {
    case PickBlocks(amount) => state.pickBlocks(amount) |> handlePickedBlocks(state) |> fetchBlocks
    case StrictPickBlocks(from, atLeastWith) =>
      // FIXME: Consider having StrictPickBlocks calls guaranteeing this
      // from parameter could be negative or 0 so we should cap it to 1 if that's the case
      val fromCapped = from.max(1)
      val minBlock = fromCapped.min(atLeastWith).max(1)
      log.debug("Strict Pick blocks from {} to {}", fromCapped, atLeastWith)
      log.debug("Lowest available block is {}", state.lowestBlock)

      val newState = if (minBlock < state.lowestBlock) {
        state.invalidateBlocksFrom(minBlock, None)._2
      } else {
        state.strictPickBlocks(fromCapped, atLeastWith) |> handlePickedBlocks(state)
      }

      fetchBlocks(newState)
    case InvalidateBlocksFrom(blockNr, reason, withBlacklist) =>
      val (blockProvider, newState) = state.invalidateBlocksFrom(blockNr, withBlacklist)
      log.debug("Invalidate blocks from {}", blockNr)
      blockProvider.foreach(peersClient ! BlacklistPeer(_, reason))
      fetchBlocks(newState)
  }

  private def handleHeadersMessages(state: BlockFetcherState): Receive = {
    case Response(peer, BlockHeaders(headers)) if state.isFetchingHeaders =>
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
      log.debug("Time-out occurred while waiting for headers")

      val newState = state.withHeaderFetchReceived
      fetchBlocks(newState)
  }

  private def handleBodiesMessages(state: BlockFetcherState): Receive = {
    case Response(peer, BlockBodies(bodies)) if state.isFetchingBodies =>
      val newState =
        if (state.fetchingBodiesState == AwaitingBodiesToBeIgnored) {
          log.debug("Received {} block bodies that will be ignored", bodies.size)
          state.withBodiesFetchReceived
        } else {
          log.debug("Fetched {} block bodies", bodies.size)
          state.withBodiesFetchReceived.addBodies(peer.id, bodies)
        }

      fetchBlocks(newState)
    case RetryBodiesRequest if state.isFetchingBodies =>
      log.debug("Time-out occurred while waiting for bodies")

      val newState = state.withBodiesFetchReceived
      fetchBlocks(newState)
  }

  private def handleStateNodeMessages(state: BlockFetcherState): Receive = {
    case FetchStateNode(hash) => fetchStateNode(hash, sender(), state)
    case RetryFetchStateNode if state.isFetchingStateNode =>
      state.stateNodeFetcher.foreach(fetcher => fetchStateNode(fetcher.hash, fetcher.replyTo, state))
    case Response(peer, NodeData(values)) if state.isFetchingStateNode =>
      log.debug("Received state node response from peer {}", peer)
      state.stateNodeFetcher.foreach(fetcher => {
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
            context become started(state.notFetchingStateNode())
        }
      })
  }

  private def handleNewBlockMessages(state: BlockFetcherState): Receive = {
    case MessageFromPeer(NewBlockHashes(hashes), _) =>
      log.debug("Received NewBlockHashes numbers {}", hashes.map(_.number).mkString(", "))
      val newState = state.validateNewBlockHashes(hashes) match {
        case Left(_) => state
        case Right(validHashes) => state.withPossibleNewTopAt(validHashes.lastOption.map(_.number))
      }
      supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
      fetchBlocks(newState)
    case MessageFromPeer(NewBlock(_, block, _), peerId) =>
      //TODO ETCM-389: Handle mined, checkpoint and new blocks uniformly
      log.debug("Received NewBlock {}", block.idTag)
      val newBlockNr = block.number
      val nextExpectedBlock = state.lastFullBlockNumber + 1

      if (state.isOnTop && newBlockNr == nextExpectedBlock) {
        log.debug("Passing block directly to importer")
        val newState = state.withPeerForBlocks(peerId, Seq(newBlockNr)).withKnownTopAt(newBlockNr)
        state.importer ! OnTop
        state.importer ! ImportNewBlock(block, peerId)
        supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
        context become started(newState)
      } else {
        log.debug("Ignoring received block as it doesn't match local state or fetch side is not on top")
        val newState = state.withPossibleNewTopAt(block.number)
        supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
        fetchBlocks(newState)
      }
    case BlockImportFailed(blockNr, reason) =>
      val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
      peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
      fetchBlocks(newState)
  }

  private def handlePossibleTopUpdate(state: BlockFetcherState): Receive = {
    //by handling these type of messages, fetcher can received from network, fresh info about blocks on top
    //ex. After a successful handshake, fetcher will receive the info about the header of the peer best block
    case MessageFromPeer(BlockHeaders(headers), _) =>
      headers.lastOption.map { bh =>
        log.debug(s"Candidate for new top at block ${bh.number}, current known top ${state.knownTop}")
        val newState = state.withPossibleNewTopAt(bh.number)
        fetchBlocks(newState)
      }
    //keep fetcher state updated in case new checkpoint block or mined block was imported
    case InternalLastBlockImport(blockNr) =>
      log.debug(s"New last block $blockNr imported from the inside")
      val newLastBlock = blockNr.max(state.lastBlock)
      val newState = state.withLastBlock(newLastBlock).withPossibleNewTopAt(blockNr)

      fetchBlocks(newState)
  }

  private def handlePickedBlocks(
      state: BlockFetcherState
  )(pickResult: Option[(NonEmptyList[Block], BlockFetcherState)]): BlockFetcherState =
    pickResult
      .tap { case (blocks, newState) =>
        sender() ! PickedBlocks(blocks)
        newState.importer ! (if (newState.isOnTop) OnTop else NotOnTop)
      }
      .fold(state)(_._2)

  private def fetchBlocks(state: BlockFetcherState): Unit = {
    // Remember that tryFetchHeaders and tryFetchBodies can issue a request
    // Nice and clean way to express that would be to use SyncIO from cats-effect
    val newState = state |> tryFetchHeaders |> tryFetchBodies

    context become started(newState)
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

    fetchHeadersFrom(blockNr, amount) pipeTo self
  }

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Future[Any] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    requestBlockHeaders(msg)
  }

  private def tryFetchBodies(fetcherState: BlockFetcherState): BlockFetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .map(state => state.withNewBodiesFetch)
      .getOrElse(fetcherState)

  private def fetchBodies(state: BlockFetcherState): Unit = {
    log.debug("Fetching bodies")

    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)
    requestBlockBodies(hashes) pipeTo self
  }

  private def fetchStateNode(hash: ByteString, originalSender: ActorRef, state: BlockFetcherState): Unit = {
    log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
    requestStateNode(hash, originalSender) pipeTo self
    val newState = state.fetchingStateNode(hash, originalSender)

    context become started(newState)
  }

  private def requestBlockHeaders(msg: GetBlockHeaders): Future[Any] =
    makeRequest(Request.create(msg, BestPeer), RetryHeadersRequest)
      .flatMap {
        case Response(_, BlockHeaders(headers)) if headers.isEmpty =>
          log.debug("Empty BlockHeaders response. Retry in {}", syncConfig.syncRetryInterval)
          Future.successful(RetryHeadersRequest).delayedBy(syncConfig.syncRetryInterval)
        case res => Future.successful(res)
      }

  private def requestBlockBodies(hashes: Seq[ByteString]): Future[Any] =
    makeRequest(Request.create(GetBlockBodies(hashes), BestPeer), RetryBodiesRequest)

  private def requestStateNode(hash: ByteString, requestor: ActorRef): Future[Any] =
    makeRequest(Request.create(GetNodeData(List(hash)), BestPeer), RetryFetchStateNode)

  private def makeRequest(request: Request[_], responseFallback: FetchMsg): Future[Any] =
    (peersClient ? request)
      .tap(blacklistPeerOnFailedRequest)
      .flatMap(failureTo(responseFallback))

  private def blacklistPeerOnFailedRequest(msg: Any): Unit = msg match {
    case RequestFailed(peer, reason) => peersClient ! BlacklistPeer(peer.id, reason)
    case _ => ()
  }

  private def failureTo(fallback: FetchMsg)(msg: Any): Future[Any] = msg match {
    case failed: RequestFailed =>
      log.debug("Failed request {}", failed)
      Future.successful(fallback)
    case Failure(cause) =>
      log.debug("Failed request due to {}", cause)
      Future.successful(fallback)
    case NoSuitablePeer =>
      Future.successful(fallback).delayedBy(syncConfig.syncRetryInterval)
    case m => Future.successful(m)
  }
}

object BlockFetcher {

  def props(
      peersClient: ActorRef,
      peerEventBus: ActorRef,
      supervisor: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(new BlockFetcher(peersClient, peerEventBus, supervisor, syncConfig, scheduler))

  sealed trait FetchMsg
  case class Start(importer: ActorRef, fromBlock: BigInt) extends FetchMsg
  case class FetchStateNode(hash: ByteString) extends FetchMsg
  case object RetryFetchStateNode extends FetchMsg
  case class PickBlocks(amount: Int) extends FetchMsg
  case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt) extends FetchMsg
  case object PrintStatus extends FetchMsg
  case class InvalidateBlocksFrom(fromBlock: BigInt, reason: String, toBlacklist: Option[BigInt]) extends FetchMsg

  object InvalidateBlocksFrom {

    def apply(from: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, if (shouldBlacklist) Some(from) else None)

    def apply(from: BigInt, reason: String, toBlacklist: Option[BigInt]): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(from, reason, toBlacklist)
  }
  case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchMsg
  case class InternalLastBlockImport(blockNr: BigInt) extends FetchMsg
  case object RetryBodiesRequest extends FetchMsg
  case object RetryHeadersRequest extends FetchMsg

  sealed trait FetchResponse
  case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchResponse
  case class FetchedStateNode(stateNode: NodeData) extends FetchResponse
}
