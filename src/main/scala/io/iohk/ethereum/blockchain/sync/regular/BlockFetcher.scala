package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Status.Failure
import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.pattern.{ask, pipe}
import akka.util.{ByteString, Timeout}
import cats.data.NonEmptyList
import cats.instances.future._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.option._
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, NotOnTop, OnTop}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import mouse.all._
import io.iohk.ethereum.utils.FutureOps._

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class BlockFetcher(
    val peersClient: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging {

  import BlockFetcher._

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 1.second // some margin for actor communication

  val printSchedule: Cancellable =
    scheduler.schedule(syncConfig.printStatusInterval, syncConfig.printStatusInterval, self, PrintStatus)

  override def receive: Receive = idle()

  override def postStop(): Unit = {
    super.postStop()
    printSchedule.cancel()
    peerEventBus ! Unsubscribe()
  }

  private def idle(): Receive = handleCommonMessages(None) orElse {
    case Start(importer, blockNr) =>
      FetcherState.initial(importer, blockNr) |> fetchBlocks
      peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))
  }

  def handleCommonMessages(state: Option[FetcherState]): Receive = {
    case PrintStatus =>
      log.info("BlockFetcher status {}", state.map(_.status))
  }

  private def started(state: FetcherState): Receive =
    handleCommonMessages(Some(state)) orElse
      handleCommands(state) orElse
      handleNewBlockMessages(state) orElse
      handleHeadersMessages(state) orElse
      handleBodiesMessages(state) orElse
      handleStateNodeMessages(state) orElse {
      case msg => log.debug("Unhandled msg {} from {}", msg, sender())
    }

  private def handleCommands(state: FetcherState): Receive = {
    case PickBlocks(amount) =>
      log.debug("Pick {} blocks", amount)
      state.pickBlocks(amount) |> handlePickedBlocks(state) |> fetchBlocks
    case StrictPickBlocks(from, atLeastWith) =>
      val minBlock = from.min(atLeastWith)
      log.debug("Strict Pick blocks from {} to {}", from, atLeastWith)
      log.debug("Lowest available block is {}", state.lowestBlock)

      val newState = if (minBlock < state.lowestBlock) {
        state.invalidateBlocksFrom(minBlock, None)._2
      } else {
        state.strictPickBlocks(from, atLeastWith) |> handlePickedBlocks(state)
      }

      fetchBlocks(newState)
    case InvalidateBlocksFrom(blockNr, reason, withBlacklist) =>
      val (blockProvider, newState) = state.invalidateBlocksFrom(blockNr, withBlacklist)

      blockProvider.foreach(peersClient ! BlacklistPeer(_, reason))

      fetchBlocks(newState)
    case FetchBlocks => fetchBlocks(state)
  }

  private def handleHeadersMessages(state: FetcherState): Receive = {
    case Response(peer, BlockHeaders(headers)) if state.isFetchingHeaders =>
      if (headers.nonEmpty) {
        log.debug("Fetched headers for blocks {} - {}", headers.head.number, headers.last.number)
      } else {
        log.debug("Received no headers")
      }

      val newState = state.validatedHeaders(headers) match {
        case Left(err) =>
          peersClient ! BlacklistPeer(peer.id, err)
          state.fetchingHeaders(false)
        case Right(validHeaders) =>
          state.appendHeaders(validHeaders)
      }

      fetchBlocks(newState)
    case RetryHeadersRequest if state.isFetchingHeaders =>
      log.debug("Retrying request for headers")
      fetchHeaders(state)
  }

  private def handleBodiesMessages(state: FetcherState): Receive = {
    case Response(peer, BlockBodies(bodies)) if state.isFetchingBodies =>
      log.debug("Fetched {} block bodies by {}", bodies.size, sender())
      state.addBodies(peer, bodies) |> fetchBlocks
    case RetryBodiesRequest if state.isFetchingBodies => fetchBodies(state)
  }

  private def handleStateNodeMessages(state: FetcherState): Receive = {
    case FetchStateNode(hash) => fetchStateNode(hash, sender(), state)
    case RetryFetchStateNode if state.isFetchingStateNode =>
      state.stateNodeFetcher.foreach(fetcher => fetchStateNode(fetcher.hash, fetcher.replyTo, state))
    case Response(peer, NodeData(values)) if state.isFetchingStateNode =>
      log.debug("Received state node response from peer {}", peer)
      state.stateNodeFetcher.foreach(fetcher => {
        val validatedNode = values
          .asRight[String]
          .ensure(s"Empty response from peer $peer, blacklisting")(_.nonEmpty)
          .ensure(s"Fetched node state hash doesn't match requested one, blacklisting peer")(nodes =>
            fetcher.hash == kec256(nodes.head))

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

  private def handleNewBlockMessages(state: FetcherState): Receive = {
    case MessageFromPeer(NewBlockHashes(hashes), _) =>
      val newState = state.validatedHashes(hashes) match {
        case Left(_) => state
        case Right(validHashes) => state.withPossibleNewTopAt(validHashes.lastOption.map(_.number))
      }

      fetchBlocks(newState)
    case MessageFromPeer(NewBlock(block, _), peerId) =>
      val newBlockNr = Block.number(block)
      val nextExpectedBlock = state.lastFullBlockNumber + 1

      log.debug("Received NewBlock nr {}", newBlockNr)

      // we're on top, so we can pass block directly to importer
      if (newBlockNr == nextExpectedBlock && state.isOnTop) {
        val newState = state.withPeerForBlocks(peerId, Seq(newBlockNr)).withKnownTopAt(newBlockNr)
        state.importer ! OnTop
        state.importer ! ImportNewBlock(block, peerId)
        context become started(newState)
        // there are some blocks waiting for import but it seems that we reached top on fetch side so we can enqueue new block for import
      } else if (newBlockNr == nextExpectedBlock && !state.isFetching && state.waitingHeaders.isEmpty) {
        val newState = state.appendNewBlock(block, peerId)
        context become started(newState)
        // waiting for some bodies but we don't have this header yet - at least we can use new block header
      } else if (newBlockNr == state.lastBlock + 1 && !state.isFetchingHeaders) {
        state.appendHeaders(List(block.header)) |> fetchBlocks
        // we're far from top
      } else if (newBlockNr > nextExpectedBlock) {
        context become started(state.withKnownTopAt(newBlockNr))
      }
    case BlockImportFailed(blockNr, reason) =>
      val (peerId, newState) = state.invalidateBlocksFrom(blockNr)
      peerId.foreach(id => peersClient ! BlacklistPeer(id, reason))
      fetchBlocks(newState)
  }

  private def handlePickedBlocks(state: FetcherState)(
      pickResult: Option[(NonEmptyList[Block], FetcherState)]): FetcherState =
    pickResult
      .tap({
        case (blocks, newState) =>
          sender() ! PickedBlocks(blocks)
          newState.importer ! (if (newState.isOnTop) OnTop else NotOnTop)
      })
      .fold(state)(_._2)

  private def fetchBlocks(state: FetcherState): Unit = {
    log.debug("Fetching blocks")
    val newState = state |> tryFetchHeaders |> tryFetchBodies

    if (!newState.isFetching && !newState.hasFetchedTopHeader) {
      scheduler.scheduleOnce(syncConfig.syncRetryInterval, context.self, FetchBlocks)
    }

    context become started(newState)
  }

  private def tryFetchHeaders(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingHeaders)
      .filter(!_.hasFetchedTopHeader)
      .tap(fetchHeaders)
      .map(_.fetchingHeaders(true))
      .getOrElse(fetcherState)

  private def fetchHeaders(state: FetcherState): Unit = {
    log.debug("Doing request for headers with BlockFetcher status {}", state.status)
    val blockNr = state.lastBlock + 1
    val amount = syncConfig.blockHeadersPerRequest

    fetchHeadersFrom(blockNr, amount) pipeTo self
  }

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Future[Any] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    requestBlockHeaders(msg)
  }

  private def tryFetchBodies(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filter(!_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .map(state => state.fetchingBodies(true))
      .getOrElse(fetcherState)

  private def fetchBodies(state: FetcherState): Unit = {
    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)
    requestBlockBodies(hashes) pipeTo self
  }

  private def fetchStateNode(hash: ByteString, originalSender: ActorRef, state: FetcherState): Unit = {
    log.debug("Fetching state node for hash {}", ByteStringUtils.hash2string(hash))
    requestStateNode(hash, originalSender) pipeTo self
    val newState = state.fetchingStateNode(hash, originalSender)

    context become started(newState)
  }

  private def requestBlockHeaders(msg: GetBlockHeaders): Future[Any] =
    makeRequest(Request.create(msg, BestPeer), RetryHeadersRequest)
      .flatMap {
        case Response(_, BlockHeaders(headers)) if headers.isEmpty =>
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

  def props(peersClient: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new BlockFetcher(peersClient, peerEventBus, syncConfig, scheduler))

  sealed trait FetchMsg
  case class Start(importer: ActorRef, fromBlock: BigInt) extends FetchMsg
  case object FetchBlocks extends FetchMsg
  case class FetchStateNode(hash: ByteString) extends FetchMsg
  case object RetryFetchStateNode extends FetchMsg
  case class PickBlocks(amount: Int) extends FetchMsg
  case class StrictPickBlocks(from: BigInt, atLEastWith: BigInt) extends FetchMsg
  case object PrintStatus extends FetchMsg
  case class InvalidateBlocksFrom(blockNr: BigInt, reason: String, toBlacklist: Option[BigInt]) extends FetchMsg

  object InvalidateBlocksFrom {

    def apply(blockNr: BigInt, reason: String, shouldBlacklist: Boolean = true): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(blockNr, reason, if (shouldBlacklist) Some(blockNr) else None)

    def apply(blockNr: BigInt, reason: String, toBlacklist: Option[BigInt]): InvalidateBlocksFrom =
      new InvalidateBlocksFrom(blockNr, reason, toBlacklist)
  }
  case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchMsg
  case object RetryBodiesRequest extends FetchMsg
  case object RetryHeadersRequest extends FetchMsg

  sealed trait FetchResponse
  case class PickedBlocks(blocks: NonEmptyList[Block]) extends FetchResponse
  case class FetchedStateNode(stateNode: NodeData) extends FetchResponse

  case class StateNodeFetcher(hash: ByteString, replyTo: ActorRef)

  case class FetcherState(
      importer: ActorRef,
      readyBlocks: Queue[Block],
      waitingHeaders: Queue[BlockHeader],
      isFetchingHeaders: Boolean,
      isFetchingBodies: Boolean,
      stateNodeFetcher: Option[StateNodeFetcher],
      lastBlock: BigInt,
      knownTop: BigInt,
      blockProviders: Map[BigInt, PeerId]
  ) {

    def isFetching: Boolean = isFetchingHeaders || isFetchingBodies

    def isFetchingStateNode: Boolean = stateNodeFetcher.isDefined

    def hasEmptyBuffer: Boolean = readyBlocks.isEmpty && waitingHeaders.isEmpty

    def hasFetchedTopHeader: Boolean = lastBlock == knownTop

    def isOnTop: Boolean = !isFetching && hasFetchedTopHeader && hasEmptyBuffer

    def lastFullBlockNumber: BigInt =
      readyBlocks.lastOption
        .map(Block.number)
        .orElse(waitingHeaders.headOption.map(_.number).map(_ - 1))
        .getOrElse(lastBlock)

    def lowestBlock: BigInt =
      readyBlocks.headOption
        .map(Block.number)
        .orElse(waitingHeaders.headOption.map(_.number))
        .getOrElse(lastBlock)

    def takeHashes(amount: Int): Seq[ByteString] = waitingHeaders.take(amount).map(_.hash)

    def appendHeaders(headers: Seq[BlockHeader]): FetcherState =
      fetchingHeaders(false)
        .withPossibleNewTopAt(headers.lastOption.map(_.number))
        .copy(
          waitingHeaders = waitingHeaders ++ headers.filter(_.number > lastBlock),
          lastBlock = HeadersSeq.lastNumber(headers).getOrElse(lastBlock)
        )

    def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] =
      if (headers.isEmpty) {
        Right(headers)
      } else {
        headers
          .asRight[String]
          .ensure("Given headers are not sequence with already fetched ones")(_.head.number <= lastBlock + 1)
          .ensure("Given headers aren't better than already fetched ones")(_.last.number > lastBlock)
          .ensure("Given headers should form a sequence without gaps")(HeadersSeq.areChain)
      }

    def validatedHashes(hashes: Seq[BlockHash]): Either[String, Seq[BlockHash]] =
      hashes
        .asRight[String]
        .ensure("Hashes are empty")(_.nonEmpty)
        .ensure("Hashes are too new")(_.head.number == lastBlock + 1)
        .ensure("Hashes should form a chain")(hashes =>
          hashes.zip(hashes.tail).forall {
            case (a, b) => a.number + 1 == b.number
        })

    def addBodies(peer: Peer, bodies: Seq[BlockBody]): FetcherState = {
      val (matching, waiting) = waitingHeaders.splitAt(bodies.length)
      val blocks = matching.zip(bodies).map((Block.apply _).tupled)

      fetchingBodies(false)
        .withPeerForBlocks(peer.id, blocks.map(_.header.number))
        .copy(readyBlocks = readyBlocks.enqueue(blocks), waitingHeaders = waiting)
    }

    def appendNewBlock(block: Block, fromPeer: PeerId): FetcherState =
      withPeerForBlocks(fromPeer, Seq(block.header.number))
        .withPossibleNewTopAt(Block.number(block))
        .copy(
          readyBlocks = readyBlocks.enqueue(block),
          waitingHeaders = waitingHeaders.filter(block.header.number != _.number)
        )

    def pickBlocks(amount: Int): Option[(NonEmptyList[Block], FetcherState)] =
      if (readyBlocks.nonEmpty) {
        val (picked, rest) = readyBlocks.splitAt(amount)
        Some((NonEmptyList(picked.head, picked.tail.toList), copy(readyBlocks = rest)))
      } else {
        None
      }

    def strictPickBlocks(from: BigInt, atLeastWith: BigInt): Option[(NonEmptyList[Block], FetcherState)] = {
      val lower = from.min(atLeastWith)
      val upper = from.max(atLeastWith)
      readyBlocks.some
        .filter(_.headOption.exists(block => Block.number(block) <= lower))
        .filter(_.lastOption.exists(block => Block.number(block) >= upper))
        .filter(_.nonEmpty)
        .map(blocks => (NonEmptyList(blocks.head, blocks.tail.toList), copy(readyBlocks = Queue())))
    }

    def invalidateBlocksFrom(nr: BigInt): (Option[PeerId], FetcherState) = invalidateBlocksFrom(nr, Some(nr))

    def invalidateBlocksFrom(nr: BigInt, toBlacklist: Option[BigInt]): (Option[PeerId], FetcherState) =
      (
        toBlacklist.flatMap(blockProviders.get),
        copy(
          readyBlocks = Queue(),
          waitingHeaders = Queue(),
          lastBlock = nr - 2,
          isFetchingHeaders = false,
          isFetchingBodies = false,
          blockProviders = blockProviders - nr
        ))

    def withLastBlock(nr: BigInt): FetcherState = copy(lastBlock = nr)

    def withKnownTopAt(nr: BigInt): FetcherState = copy(knownTop = nr)

    def withPossibleNewTopAt(nr: BigInt): FetcherState =
      if (nr > knownTop) {
        withKnownTopAt(nr)
      } else {
        this
      }
    def withPossibleNewTopAt(nr: Option[BigInt]): FetcherState = nr.map(withPossibleNewTopAt).getOrElse(this)

    def withPeerForBlocks(peerId: PeerId, blocks: Seq[BigInt]): FetcherState =
      copy(
        blockProviders = blockProviders ++ blocks.map(block => block -> peerId)
      )

    def fetchingHeaders(isFetching: Boolean): FetcherState = copy(isFetchingHeaders = isFetching)

    def fetchingBodies(isFetching: Boolean): FetcherState = copy(isFetchingBodies = isFetching)

    def fetchingStateNode(hash: ByteString, requestor: ActorRef): FetcherState =
      copy(stateNodeFetcher = Some(StateNodeFetcher(hash, requestor)))

    def notFetchingStateNode(): FetcherState = copy(stateNodeFetcher = None)

    def status: Map[String, Any] = Map(
      "ready blocks" -> readyBlocks.size,
      "fetched headers" -> waitingHeaders.size,
      "fetching headers" -> isFetchingHeaders,
      "fetching bodies" -> isFetchingBodies,
      "fetching state node" -> isFetchingStateNode,
      "fetched top header" -> hasFetchedTopHeader,
      "last block" -> lastBlock,
      "known top" -> knownTop,
      "is on top" -> isOnTop
    )
  }

  object FetcherState {

    def initial(importer: ActorRef, lastBlock: BigInt): FetcherState = FetcherState(
      importer = importer,
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      isFetchingHeaders = false,
      isFetchingBodies = false,
      stateNodeFetcher = None,
      lastBlock = lastBlock,
      knownTop = lastBlock + 1,
      blockProviders = Map()
    )
  }
}
