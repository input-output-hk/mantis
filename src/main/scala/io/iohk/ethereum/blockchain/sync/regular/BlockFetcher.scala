package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.util.ByteString
import cats.Eq
import cats.syntax.either._
import cats.syntax.eq._
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.{RequestFailed, ResponseReceived}
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, NotOnTop, OnTop}
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, PeerRequestHandler}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockHeaders}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders => BlockHeadersMessage, _}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.utils.Config.SyncConfig
import mouse.all._

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.reflect.ClassTag

class BlockFetcher(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import BlockFetcher._

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

  def handleCommonMessages(state: Option[FetcherState]): Receive =
    handlePeerListMessages orElse handleBlacklistMessages orElse {
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
      blacklist(blockProvider.id, syncConfig.blacklistDuration, reason)
      fetchBlocks(newState)
    case FetchBlocks => fetchBlocks(state)
  }

  private def handleHeadersMessages(state: FetcherState): Receive = {
    case ResponseReceived(peer, BlockHeadersMessage(headers), _) if state.isHeadersFetcher(sender()) =>
      log.debug("Fetched headers for blocks {} by {}", headers.map(_.number).mkString(","), sender())

      val newState = state.validatedHeaders(headers) match {
        case Left(err) =>
          blacklistIfHandshaked(peer, err)
          state.withoutHeadersFetcher()
        case Right(validHeaders) =>
          state.appendHeaders(validHeaders)
      }

      fetchBlocks(newState)
    case RequestFailed(peer, reason) if state.isHeadersFetcher(sender()) =>
      log.debug(s"Failed headers request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      state.withoutHeadersFetcher() |> fetchBlocks
  }

  private def handleBodiesMessages(state: FetcherState): Receive = {
    case ResponseReceived(peer, BlockBodies(bodies), _) if state.isBodiesFetcher(sender()) =>
      log.debug("Fetched {} block bodies by {}", bodies.size, sender())
      state.addBodies(peer, bodies) |> fetchBlocks
    case RequestFailed(peer, reason) if state.isBodiesFetcher(sender()) =>
      log.debug(s"Failed bodies request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      state.withoutBodiesFetcher() |> fetchBlocks
  }

  private def handleStateNodeMessages(state: FetcherState): Receive = {
    case FetchStateNode(hash) => fetchStateNode(hash, sender(), state)
    case RetryFetchStateNode(hash, replyTo) => fetchStateNode(hash, replyTo, state)
    case ResponseReceived(peer, NodeData(values), _) if state.isStateNodeFetcher(sender()) =>
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
            blacklist(peer.id, syncConfig.blacklistDuration, err)
            fetchStateNode(fetcher.hash, fetcher.replyTo, state)
          case Right(node) =>
            fetcher.replyTo ! FetchedStateNode(NodeData(node))
            context become started(state.withoutStateNodeFetcher())
        }
      })
    case RequestFailed(peer, reason) if state.isStateNodeFetcher(sender()) =>
      log.debug(s"Failed state node request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      state.stateNodeFetcher.foreach(fetcher => {
        fetchStateNode(fetcher.hash, fetcher.replyTo, state.withoutHeadersFetcher())
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
        val newState =
          peerById(peerId).fold(state)(peer => state.withPeerForBlocks(peer, Seq(Block.number(block))))
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
      .flatMap(state => {
        val blockNr = state.lastBlock + 1
        val amount = math.min(syncConfig.blockHeadersPerRequest, state.missingToFillBuffer(bufferSize))
        fetchHeadersFrom(blockNr, amount).map(_.apply(state))
      })
      .getOrElse(fetcherState)

  private def tryFetchBodies(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filterNot(_.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .flatMap(state => bestPeer.map(peer => (state, peer)))
      .map {
        case (state, peer) =>
          val bodiesFetcher: ActorRef = state |>
            (_.takeHashes(syncConfig.blockBodiesPerRequest)) |>
            (requestBlockBodies(peer, _))
          state.withBodiesFetcher(bodiesFetcher)
      }
      .getOrElse(fetcherState)

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Option[FetcherState => FetcherState] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    bestPeer.map(requestBlockHeaders(_, msg)).map(fetcher => state => state.withHeadersFetcher(fetcher))
  }

  private def fetchStateNode(hash: ByteString, originalSender: ActorRef, state: FetcherState): Unit =
    bestPeer.fold(
      {
        log.debug("No peer for fetching state node, retry in 1 second")
        scheduler.scheduleOnce(1.second, self, RetryFetchStateNode(hash, originalSender))
        ()
      }
    )(peer => {
      val fetcherActor = requestStateNode(peer, hash)
      val fetcher = StateNodeFetcher(fetcherActor, hash, originalSender)
      val newState = state.withStateNodeFetcher(fetcher)
      context become started(newState)
    })

  private def requestBlockHeaders(peer: Peer, msg: GetBlockHeaders): ActorRef =
    makeRequest[GetBlockHeaders, BlockHeadersMessage](peer, msg, BlockHeadersMessage.code)

  private def requestBlockBodies(peer: Peer, hashes: Seq[ByteString]): ActorRef =
    makeRequest[GetBlockBodies, BlockBodies](peer, GetBlockBodies(hashes), BlockBodies.code)

  private def requestStateNode(peer: Peer, hash: ByteString): ActorRef =
    makeRequest[GetNodeData, NodeData](peer, GetNodeData(List(hash)), NodeData.code)

  private def makeRequest[RequestMsg <: Message, ResponseMsg <: Message: ClassTag](
      peer: Peer,
      requestMsg: RequestMsg,
      responseMsgCode: Int)(
      implicit scheduler: Scheduler,
      toSerializable: RequestMsg => MessageSerializable): ActorRef = {
    context.actorOf(
      PeerRequestHandler.props[RequestMsg, ResponseMsg](
        peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = requestMsg,
        responseMsgCode = responseMsgCode
      ))
  }

  private def bestPeer: Option[Peer] = {
    log.debug("available peers {}", peersToDownloadFrom)
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, PeerInfo(_, totalDifficulty, true, _)) =>
          (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) {
      val (peer, _) = peersToUse.maxBy { case (_, td) => td }
      Some(peer)
    } else {
      None
    }
  }

}
object BlockFetcher {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new BlockFetcher(etcPeerManager, peerEventBus, syncConfig, scheduler))

  sealed trait FetchMsg
  case class Start(importer: ActorRef, fromBlock: BigInt) extends FetchMsg
  case object FetchBlocks extends FetchMsg
  case class FetchStateNode(hash: ByteString) extends FetchMsg
  case class RetryFetchStateNode(hash: ByteString, replyTo: ActorRef) extends FetchMsg
  case class PickBlocks(amount: Int) extends FetchMsg
  case object PrintStatus extends FetchMsg
  case class InvalidateBlocksFrom(blockNr: BigInt, reason: String) extends FetchMsg
  case class BlockImportFailed(blockNr: BigInt, reason: String) extends FetchMsg

  sealed trait FetchResponse
  case class PickedBlocks(blocks: List[Block]) extends FetchResponse
  case class FetchedStateNode(stateNode: NodeData) extends FetchResponse

  case class StateNodeFetcher(theFetcher: ActorRef, hash: ByteString, replyTo: ActorRef)

  case class FetcherState(
      importer: ActorRef,
      readyBlocks: Queue[Block],
      waitingHeaders: Queue[BlockHeader],
      headersFetcher: Option[ActorRef],
      bodiesFetcher: Option[ActorRef],
      stateNodeFetcher: Option[StateNodeFetcher],
      hasFetchedTopHeader: Boolean,
      lastBlock: BigInt,
      blockProviders: Map[BigInt, Peer],
  ) {

    def isFetchingHeaders: Boolean =
      headersFetcher.isDefined

    def isFetchingBodies: Boolean =
      bodiesFetcher.isDefined

    def isFetching: Boolean =
      isFetchingHeaders || isFetchingBodies

    def isHeadersFetcher(actor: ActorRef): Boolean =
      headersFetcher.fold(false)(_ === actor)

    def isBodiesFetcher(actor: ActorRef): Boolean =
      bodiesFetcher.fold(false)(_ === actor)

    def isStateNodeFetcher(actor: ActorRef): Boolean =
      stateNodeFetcher.fold(false)(_.theFetcher === actor)

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
        withoutHeadersFetcher().copy(
          waitingHeaders = waitingHeaders ++ headers,
          lastBlock = BlockHeaders.lastNumber(headers).getOrElse(lastBlock),
          hasFetchedTopHeader = false,
        )
      } else {
        fetchedTopHeader().withoutHeadersFetcher()
      }

    def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] = {
      if (headers.isEmpty) {
        Right(headers)
      } else {
        headers
          .asRight[String]
          .ensure("Given headers are not sequence with previous ones")(headers => headers.head.number == lastBlock + 1)
          .ensure("Given headers should form a sequence without gaps")(BlockHeaders.areChain)
      }
    }

    def addBodies(peer: Peer, bodies: Seq[BlockBody]): FetcherState = {
      val (matching, waiting) = waitingHeaders.splitAt(bodies.length)
      val blocks = matching.zip(bodies).map((Block.apply _).tupled)

      withoutBodiesFetcher()
        .withPeerForBlocks(peer, blocks.map(_.header.number))
        .copy(
          readyBlocks = readyBlocks.enqueue(blocks),
          waitingHeaders = waiting,
        )
    }

    def appendNewBlock(block: Block, fromPeer: Peer): FetcherState = {
      withPeerForBlocks(fromPeer, Seq(block.header.number)).copy(
        readyBlocks = readyBlocks.enqueue(block),
        waitingHeaders = waitingHeaders.filter(block.header.number != _.number)
      )
    }

    def pickBlocks(amount: Int): (Queue[Block], FetcherState) = {
      val (picked, rest) = readyBlocks.splitAt(amount)
      (picked, copy(readyBlocks = rest))
    }

    def invalidateBlocksFrom(nr: BigInt): (Peer, FetcherState) =
      (
        blockProviders(nr),
        copy(
          readyBlocks = readyBlocks.takeWhile(_.header.number < nr),
          waitingHeaders = waitingHeaders.takeWhile(_.number < nr),
          lastBlock = nr - 1,
          headersFetcher = None,
          bodiesFetcher = None,
          blockProviders = blockProviders - nr
        ))

    def withLastBlock(nr: BigInt): FetcherState = copy(lastBlock = nr)

    def withPeerForBlocks(peer: Peer, blocks: Seq[BigInt]): FetcherState =
      copy(
        blockProviders = blockProviders ++ blocks.map(block => block -> peer)
      )

    def withHeadersFetcher(fetcher: ActorRef): FetcherState =
      copy(headersFetcher = Some(fetcher))

    def withoutHeadersFetcher(): FetcherState = copy(headersFetcher = None)

    def withBodiesFetcher(fetcher: ActorRef): FetcherState =
      copy(bodiesFetcher = Some(fetcher))

    def withoutBodiesFetcher(): FetcherState = copy(bodiesFetcher = None)

    def withStateNodeFetcher(fetcher: StateNodeFetcher): FetcherState =
      copy(stateNodeFetcher = Some(fetcher))

    def withoutStateNodeFetcher(): FetcherState = copy(stateNodeFetcher = None)

    def fetchedTopHeader(): FetcherState = copy(hasFetchedTopHeader = true)

    def notFetchedTopHeader(): FetcherState = copy(hasFetchedTopHeader = false)

    def status: Map[String, Any] = Map(
      "ready blocks" -> readyBlocks.size,
      "fetched headers" -> waitingHeaders.size,
      "fetching headers" -> headersFetcher.fold("no")(actor => s"yes, from $actor"),
      "fetching bodies" -> bodiesFetcher.fold("no")(actor => s"yes, from $actor"),
      "fetching state node" -> stateNodeFetcher.fold("no")(fetcher => s"yes, from ${fetcher.theFetcher}"),
      "fetched top header" -> hasFetchedTopHeader,
      "last block" -> lastBlock,
    )
  }
  object FetcherState {
    def initial(importer: ActorRef, lastBlock: BigInt) = FetcherState(
      importer = importer,
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      headersFetcher = None,
      bodiesFetcher = None,
      stateNodeFetcher = None,
      hasFetchedTopHeader = false,
      lastBlock = lastBlock,
      blockProviders = Map(),
    )
  }

  implicit val actorEq: Eq[ActorRef] = _ equals _
}
