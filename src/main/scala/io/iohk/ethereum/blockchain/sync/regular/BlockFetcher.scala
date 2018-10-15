package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.pattern.{AskTimeoutException, ask, pipe}
import akka.util.{ByteString, Timeout}
import cats.instances.future._
import cats.instances.option._
import cats.syntax.either._
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
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import mouse.all._

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class BlockFetcher(
    val peersClient: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging {

  import BlockFetcher._

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout
    : Timeout = syncConfig.peerResponseTimeout + 100.milliseconds // some margin for actor communication

  val bufferSize = 10000

  val printSchedule: Cancellable = scheduler.schedule(1.second, 1.second, self, PrintStatus)
  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))

  override def receive: Receive = idle()

  override def postStop(): Unit = {
    super.postStop()
    printSchedule.cancel()
    peerEventBus ! Unsubscribe()
  }

  private def idle(): Receive =
    handleCommonMessages(None) orElse {
      case Start(importer, blockNr) => FetcherState.initial(importer, blockNr) |> fetchBlocks
    }

  def handleCommonMessages(state: Option[FetcherState]): Receive = {
    case PrintStatus =>
      log.debug("BlockFetcher status {}", state.map(_.status))
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
      val (blocks, newState) = state.pickBlocks(amount)
      if (blocks.nonEmpty) {
        sender() ! PickedBlocks(blocks.toList)
      }
      val topMsg = if (state.isOnTop) OnTop else NotOnTop
      newState.importer ! topMsg
      fetchBlocks(newState)
    case InvalidateBlocksFrom(blockNr, reason) =>
      val (blockProvider, newState) = state.invalidateBlocksFrom(blockNr)
      peersClient ! BlacklistPeer(blockProvider, reason)
      fetchBlocks(newState)
    case FetchBlocks => fetchBlocks(state)
  }

  private def handleHeadersMessages(state: FetcherState): Receive = {
    case Response(peer, BlockHeaders(headers)) if state.isFetchingHeaders =>
      log.debug("Fetched headers for blocks {} by {}", headers.map(_.number).mkString(","), sender())

      val newState = state.validatedHeaders(headers) match {
        case Left(err) =>
          peersClient ! BlacklistPeer(peer.id, err)
          state.fetchingHeaders(false)
        case Right(validHeaders) =>
          state.appendHeaders(validHeaders)
      }

      fetchBlocks(newState)
    case RetryHeadersRequest if state.isFetchingHeaders => fetchHeaders(state)
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
    case MessageFromPeer(NewBlockHashes(_), _) =>
      val newState = state.notFetchedTopHeader()
      fetchBlocks(newState)
    case MessageFromPeer(NewBlock(block, _), peerId) if state.isOnTop =>
      val lastReadyBlock = state.lastReadyBlockNumber
      if (Block.number(block) == lastReadyBlock + 1) {
        state.importer ! ImportNewBlock(block, peerId)
        val newState = state.withPeerForBlocks(peerId, Seq(Block.number(block)))
        context become started(newState)
      } else {
        val newState = state.notFetchedTopHeader()
        fetchBlocks(newState)
      }
  }

  private def fetchBlocks(state: FetcherState): Unit = {
    val newState = state |> tryFetchHeaders |> tryFetchBodies

    if (!state.isFetching && !newState.hasFetchedTopHeader) {
      scheduler.scheduleOnce(10.seconds, context.self, FetchBlocks)
    }

    context become started(newState)
  }

  private def tryFetchHeaders(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filterNot(_.isFetchingHeaders)
      .filterNot(_.hasReachedBufferSize(bufferSize))
      .filterNot(_.hasFetchedTopHeader)
      .tap(fetchHeaders)
      .map(_.fetchingHeaders(true))
      .getOrElse(fetcherState)

  private def fetchHeaders(state: FetcherState): Unit = {
    val blockNr = state.lastBlock + 1
    val amount = syncConfig.blockHeadersPerRequest

    fetchHeadersFrom(blockNr, amount) pipeTo self
  }

  private def tryFetchBodies(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filterNot(_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .tap(fetchBodies)
      .map(state => state.fetchingBodies(true))
      .getOrElse(fetcherState)

  private def fetchBodies(state: FetcherState): Unit = {
    val hashes = state.takeHashes(syncConfig.blockBodiesPerRequest)
    requestBlockBodies(hashes) pipeTo self
  }

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Future[Any] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    requestBlockHeaders(msg)
  }

  private def fetchStateNode(hash: ByteString, originalSender: ActorRef, state: FetcherState): Unit = {
    requestStateNode(hash, originalSender) pipeTo self
    val newState = state.fetchingStateNode(hash, originalSender)

    context become started(newState)
  }

  private def requestBlockHeaders(msg: GetBlockHeaders): Future[Any] =
    makeRequest(Request.create(msg, BestPeer))

  private def requestBlockBodies(hashes: Seq[ByteString]): Future[Any] =
    makeRequest(Request.create(GetBlockBodies(hashes), BestPeer))

  private def requestStateNode(hash: ByteString, requestor: ActorRef): Future[Any] =
    makeRequest(Request.create(GetNodeData(List(hash)), BestPeer))

  private def makeRequest(request: Request[_]): Future[Any] =
    (peersClient ? request)
      .tap(blacklistPeerOnFailedRequest)
      .map(failureTo(RetryFetchStateNode))

  private def blacklistPeerOnFailedRequest(msg: Any): Unit = msg match {
    case RequestFailed(peer, reason) => peersClient ! BlacklistPeer(peer.id, reason)
  }

  private def failureTo(fallback: FetchMsg)(msg: Any): Any = msg match {
    case _: RequestFailed => fallback
    case _: AskTimeoutException => fallback
    case NoSuitablePeer => fallback
    case msg: Response[_] => msg
  }
}
object BlockFetcher {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new BlockFetcher(etcPeerManager, peerEventBus, syncConfig, scheduler))

  sealed trait FetchMsg
  case class Start(importer: ActorRef, fromBlock: BigInt) extends FetchMsg
  case object FetchBlocks extends FetchMsg
  case class FetchStateNode(hash: ByteString) extends FetchMsg
  case object RetryFetchStateNode extends FetchMsg
  case class PickBlocks(amount: Int) extends FetchMsg
  case object PrintStatus extends FetchMsg
  case class InvalidateBlocksFrom(blockNr: BigInt, reason: String) extends FetchMsg
  case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchMsg
  case object RetryBodiesRequest extends FetchMsg
  case object RetryHeadersRequest extends FetchMsg

  sealed trait FetchResponse
  case class PickedBlocks(blocks: List[Block]) extends FetchResponse
  case class FetchedStateNode(stateNode: NodeData) extends FetchResponse

  case class StateNodeFetcher(hash: ByteString, replyTo: ActorRef)

  case class FetcherState(
      importer: ActorRef,
      readyBlocks: Queue[Block],
      waitingHeaders: Queue[BlockHeader],
      isFetchingHeaders: Boolean,
      isFetchingBodies: Boolean,
      stateNodeFetcher: Option[StateNodeFetcher],
      hasFetchedTopHeader: Boolean,
      lastBlock: BigInt,
      blockProviders: Map[BigInt, PeerId],
  ) {

    def isFetching: Boolean =
      isFetchingHeaders || isFetchingBodies

    def hasReachedBufferSize(bufferSize: Int): Boolean =
      readyBlocks.size >= bufferSize

    def missingToFillBuffer(bufferSize: Int): Int =
      math.max(0, bufferSize - readyBlocks.size)

    def hasEmptyBuffer: Boolean =
      readyBlocks.isEmpty

    def isOnTop: Boolean = !isFetching && hasFetchedTopHeader && hasEmptyBuffer

    def lastReadyBlockNumber: BigInt =
      readyBlocks.lastOption
        .map(Block.number)
        .orElse(waitingHeaders.headOption.map(_.number).map(_ - 1))
        .getOrElse(lastBlock)

    def takeHashes(amount: Int): Seq[ByteString] =
      waitingHeaders.take(amount).map(_.hash)

    def appendHeaders(headers: Seq[BlockHeader]): FetcherState =
      if (headers.nonEmpty) {
        fetchingHeaders(false).copy(
          waitingHeaders = waitingHeaders ++ headers,
          lastBlock = HeadersSeq.lastNumber(headers).getOrElse(lastBlock),
          hasFetchedTopHeader = false,
        )
      } else {
        fetchedTopHeader().fetchingHeaders(false)
      }

    def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] = {
      if (headers.isEmpty) {
        Right(headers)
      } else {
        headers
          .asRight[String]
          .ensure("Given headers are not sequence with previous ones")(headers => headers.head.number == lastBlock + 1)
          .ensure("Given headers should form a sequence without gaps")(HeadersSeq.areChain)
      }
    }

    def addBodies(peer: Peer, bodies: Seq[BlockBody]): FetcherState = {
      val (matching, waiting) = waitingHeaders.splitAt(bodies.length)
      val blocks = matching.zip(bodies).map((Block.apply _).tupled)

      fetchingBodies(false)
        .withPeerForBlocks(peer.id, blocks.map(_.header.number))
        .copy(
          readyBlocks = readyBlocks.enqueue(blocks),
          waitingHeaders = waiting,
        )
    }

    def appendNewBlock(block: Block, fromPeer: Peer): FetcherState = {
      withPeerForBlocks(fromPeer.id, Seq(block.header.number)).copy(
        readyBlocks = readyBlocks.enqueue(block),
        waitingHeaders = waitingHeaders.filter(block.header.number != _.number)
      )
    }

    def pickBlocks(amount: Int): (Queue[Block], FetcherState) = {
      val (picked, rest) = readyBlocks.splitAt(amount)
      (picked, copy(readyBlocks = rest))
    }

    def invalidateBlocksFrom(nr: BigInt): (PeerId, FetcherState) =
      (
        blockProviders(nr),
        copy(
          readyBlocks = readyBlocks.takeWhile(_.header.number < nr),
          waitingHeaders = waitingHeaders.takeWhile(_.number < nr),
          lastBlock = nr - 1,
          isFetchingHeaders = false,
          isFetchingBodies = false,
          blockProviders = blockProviders - nr
        ))

    def withLastBlock(nr: BigInt): FetcherState = copy(lastBlock = nr)

    def withPeerForBlocks(peerId: PeerId, blocks: Seq[BigInt]): FetcherState =
      copy(
        blockProviders = blockProviders ++ blocks.map(block => block -> peerId)
      )

    def fetchingHeaders(isFetching: Boolean): FetcherState = copy(isFetchingHeaders = isFetching)

    def fetchingBodies(isFetching: Boolean): FetcherState = copy(isFetchingBodies = isFetching)

    def fetchingStateNode(hash: ByteString, requestor: ActorRef): FetcherState =
      copy(stateNodeFetcher = Some(StateNodeFetcher(hash, requestor)))

    def notFetchingStateNode(): FetcherState = copy(stateNodeFetcher = None)

    def isFetchingStateNode: Boolean = stateNodeFetcher.isDefined

    def fetchedTopHeader(): FetcherState = copy(hasFetchedTopHeader = true)

    def notFetchedTopHeader(): FetcherState = copy(hasFetchedTopHeader = false)

    def status: Map[String, Any] = Map(
      "ready blocks" -> readyBlocks.size,
      "fetched headers" -> waitingHeaders.size,
      "fetching headers" -> isFetchingHeaders,
      "fetching bodies" -> isFetchingBodies,
      "fetching state node" -> isFetchingStateNode,
      "fetched top header" -> hasFetchedTopHeader,
      "last block" -> lastBlock,
    )
  }
  object FetcherState {
    def initial(importer: ActorRef, lastBlock: BigInt) = FetcherState(
      importer = importer,
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      isFetchingHeaders = false,
      isFetchingBodies = false,
      stateNodeFetcher = None,
      hasFetchedTopHeader = false,
      lastBlock = lastBlock,
      blockProviders = Map(),
    )
  }
}
