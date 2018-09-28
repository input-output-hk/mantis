package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import akka.util.ByteString
import cats.Eq
import cats.syntax.either._
import cats.syntax.eq._
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.{RequestFailed, ResponseReceived}
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{ImportNewBlock, ImporterMsg, NotOnTop, OnTop}
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, PeerRequestHandler}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.TotalDifficultyStorage.TotalDifficulty
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
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

  scheduler.schedule(1.second, 1.second, self, PrintStatus)
  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))

  override def receive: Receive = idle(FetcherState.initial)

  private def idle(state: FetcherState): Receive =
    handleCommonMessages(state, idle) orElse {
      case Start(blockNr) => state |> FetcherState.withLastBlock(blockNr) |> fetchBlocks
    }

  def handleCommonMessages(state: FetcherState, currentBehavior: FetcherState => Receive): Receive =
    handlePeerListMessages orElse handleBlacklistMessages orElse {
      case ImImporter(importer) => context become currentBehavior(FetcherState.withImporter(importer, state))
      case PrintStatus => log.debug("BlockFetcher status {}", FetcherState.status(state))
    }

  private def started(state: FetcherState): Receive =
    handleCommonMessages(state, started) orElse
      handleCommands(state) orElse
      handleNewBlockMessages(state) orElse
      handleHeadersMessages(state) orElse
      handleBodiesMessages(state) orElse
      handleStateNodeMessages(state) orElse {
      case msg => log.debug("Unhandled msg {} from {}", msg, sender())
    }

  private def handleCommands(state: FetcherState): Receive = {
    case PickBlocks(amount) =>
      val (blocks, newState) = FetcherState.pickBlocks(amount)(state)
      if (blocks.nonEmpty) {
        sender() ! PickedBlocks(blocks.toList)
      }
      val topMsg = if (FetcherState.isOnTop(state)) OnTop else NotOnTop
      sendMsgToImporter(topMsg, newState)
      fetchBlocks(newState)
    case InvalidateBlocksFrom(blockNr, reason) =>
      val (blockProvider, newState) = FetcherState.invalidateBlocksFrom(blockNr)(state)
      blacklist(blockProvider.id, syncConfig.blacklistDuration, reason)
      fetchBlocks(newState)
    case FetchBlocks() => fetchBlocks(state)
  }

  private def handleHeadersMessages(state: FetcherState): Receive = {
    case ResponseReceived(_, BlockHeaders(headers), _) if FetcherState.isHeadersFetcher(sender())(state) =>
      log.debug("Fetched headers for blocks {} by {}", headers.map(_.number).mkString(","), sender())
      val newState = FetcherState.appendHeaders(headers)(state)
      fetchBlocks(newState)
    case RequestFailed(peer, reason) if FetcherState.isHeadersFetcher(sender())(state) =>
      log.debug(s"Failed headers request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      val newState = FetcherState.withoutHeadersFetcher(state)
      fetchBlocks(newState)
  }

  private def handleBodiesMessages(state: FetcherState): Receive = {
    case ResponseReceived(peer, BlockBodies(bodies), _) if FetcherState.isBodiesFetcher(sender())(state) =>
      log.debug("Fetched {} block bodies by {}", bodies.size, sender())
      val newState = FetcherState.addBodies(peer, bodies)(state)
      fetchBlocks(newState)
    case RequestFailed(peer, reason) if FetcherState.isBodiesFetcher(sender())(state) =>
      log.debug(s"Failed bodies request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      val newState = FetcherState.withoutBodiesFetcher(state)
      fetchBlocks(newState)
  }

  private def handleStateNodeMessages(state: FetcherState): Receive = {
    case FetchStateNode(hash) => fetchStateNode(hash, sender(), state)
    case RetryFetchStateNode(hash, replyTo) => fetchStateNode(hash, replyTo, state)
    case ResponseReceived(peer, NodeData(values), _) if FetcherState.isStateNodeFetcher(sender(), state) =>
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
            context become started(FetcherState.withoutStateNodeFetcher(state))
        }
      })
    case RequestFailed(peer, reason) if FetcherState.isStateNodeFetcher(sender(), state) =>
      log.debug(s"Failed state node request from peer {} with reason {}", peer, reason)
      blacklistIfHandshaked(peer, reason)
      state.stateNodeFetcher.foreach(fetcher => {
        fetchStateNode(fetcher.hash, fetcher.replyTo, FetcherState.withoutHeadersFetcher(state))
      })
  }

  private def handleNewBlockMessages(state: FetcherState): Receive = {
    case MessageFromPeer(NewBlockHashes(_), _) =>
      val newState = FetcherState.hasNotFetchedTop(state)
      fetchBlocks(newState)
    case MessageFromPeer(NewBlock(block, _), peerId) if FetcherState.isOnTop(state) =>
      val lastReadyBlock = FetcherState.lastReadyBlockNumber(state)
      if (Block.number(block) == lastReadyBlock + 1) {
        sendMsgToImporter(ImportNewBlock(block), state)
        val newState = peerById(peerId).fold(state)(peer => FetcherState.withPeerForBlocks(peer, Seq(Block.number(block)))(state))
        context become started(newState)
      } else {
        val newState = FetcherState.hasNotFetchedTop(state)
        fetchBlocks(newState)
      }
  }

  private def fetchBlocks(state: FetcherState): Unit = {
    val newState = state |> tryFetchHeaders |> tryFetchBodies

    if (!FetcherState.isFetching(newState) && !newState.fetchedTop) {
      scheduler.scheduleOnce(1.second, context.self, FetchBlocks())
    }

    context become started(newState)
  }

  private def tryFetchHeaders(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filterNot(FetcherState.isFetchingHeaders)
      .filterNot(FetcherState.hasReachedBufferSize(bufferSize))
      .filterNot(_.fetchedTop)
      .flatMap(state => {
        val blockNr = state.lastBlock + 1
        val amount = math.min(syncConfig.blockHeadersPerRequest, FetcherState.missingToFillBuffer(bufferSize)(state))
        fetchHeadersFrom(blockNr, amount).map(_.apply(state))
      })
      .getOrElse(fetcherState)

  private def tryFetchBodies(fetcherState: FetcherState): FetcherState =
    Some(fetcherState)
      .filterNot(FetcherState.isFetchingBodies)
      .filter(_.waitingHeaders.nonEmpty)
      .flatMap(state => bestPeer.map(peer => (state, peer)))
      .map {
        case (state, peer) =>
          val bodiesFetcher: ActorRef = state |>
            FetcherState.takeHashes(syncConfig.blockBodiesPerRequest) |>
            (requestBlockBodies(peer, _))
          FetcherState.withBodiesFetcher(bodiesFetcher)(state)
      }
      .getOrElse(fetcherState)

  private def fetchHeadersFrom(blockNr: BigInt, amount: Int): Option[FetcherState => FetcherState] = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    bestPeer.map(requestBlockHeaders(_, msg)).map(FetcherState.withHeadersFetcher)
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
      val newState = FetcherState.withStateNodeFetcher(fetcher, state)
      context become started(newState)
    })

  private def requestBlockHeaders(peer: Peer, msg: GetBlockHeaders): ActorRef =
    makeRequest[GetBlockHeaders, BlockHeaders](peer, msg, BlockHeaders.code)

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

  private def sendMsgToImporter(msg: ImporterMsg, state: FetcherState): Unit = state.importer.foreach(_ ! msg)
}
object BlockFetcher {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new BlockFetcher(etcPeerManager, peerEventBus, syncConfig, scheduler))

  sealed trait FetchMsg
  case class Start(fromBlock: BigInt) extends FetchMsg
  case class ImImporter(importer: ActorRef) extends FetchMsg
  case class FetchBlocks() extends FetchMsg
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
      importer: Option[ActorRef],
      readyBlocks: Queue[Block],
      waitingHeaders: Queue[BlockHeader],
      headersFetcher: Option[ActorRef],
      bodiesFetcher: Option[ActorRef],
      stateNodeFetcher: Option[StateNodeFetcher],
      fetchedTop: Boolean,
      lastBlock: BigInt,
      blockProviders: Map[BigInt, Peer],
  )

  object FetcherState {
    def initial = FetcherState(
      importer = None,
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      headersFetcher = None,
      bodiesFetcher = None,
      stateNodeFetcher = None,
      fetchedTop = false,
      lastBlock = 0,
      blockProviders = Map(),
    )

    def isFetchingHeaders(state: FetcherState): Boolean =
      state.headersFetcher.isDefined

    def isFetchingBodies(state: FetcherState): Boolean =
      state.bodiesFetcher.isDefined

    def isFetching(state: FetcherState): Boolean =
      isFetchingHeaders(state) || isFetchingBodies(state)

    def isHeadersFetcher(actor: ActorRef)(state: FetcherState): Boolean =
      state.headersFetcher.fold(false)(_ === actor)

    def isBodiesFetcher(actor: ActorRef)(state: FetcherState): Boolean =
      state.bodiesFetcher.fold(false)(_ === actor)

    def isStateNodeFetcher(actor: ActorRef, state: FetcherState): Boolean =
      state.stateNodeFetcher.fold(false)(_.theFetcher === actor)

    def hasReachedBufferSize(bufferSize: Int)(state: FetcherState): Boolean =
      state.readyBlocks.size >= bufferSize

    def missingToFillBuffer(bufferSize: Int)(state: FetcherState): Int =
      math.max(0, bufferSize - state.readyBlocks.size)

    def hasEmptyBuffer(state: FetcherState): Boolean =
      state.readyBlocks.isEmpty

    def isOnTop(state: FetcherState): Boolean = !isFetching(state) && state.fetchedTop && hasEmptyBuffer(state)

    def lastReadyBlockNumber(state: FetcherState): BigInt =
      state.readyBlocks.lastOption
        .map(Block.number)
        .orElse(state.waitingHeaders.headOption.map(_.number).map(_ - 1))
        .getOrElse(state.lastBlock)

    def takeHashes(amount: Int)(state: FetcherState): Seq[ByteString] =
      state.waitingHeaders.take(amount).map(_.hash)

    def appendHeaders(headers: Seq[BlockHeader])(state: FetcherState): FetcherState =
      if (headers.nonEmpty)
        withoutHeadersFetcher(state).copy(
          waitingHeaders = state.waitingHeaders ++ headers,
          lastBlock = headers.lastNumber.getOrElse(state.lastBlock),
          fetchedTop = false,
        )
      else
        state |> hasFetchedTop |> withoutHeadersFetcher

    def addBodies(peer: Peer, bodies: Seq[BlockBody])(state: FetcherState): FetcherState = {
      val (matching, waiting) = state.waitingHeaders.splitAt(bodies.length)
      val blocks = matching.zip(bodies).map((Block.apply _).tupled)

      state |> withoutBodiesFetcher |> withPeerForBlocks(peer, blocks.map(_.header.number)) |> (_.copy(
        readyBlocks = state.readyBlocks.enqueue(blocks),
        waitingHeaders = waiting,
      ))
    }

    def appendNewBlock(block: Block, fromPeer: Peer)(state: FetcherState): FetcherState = {
      withPeerForBlocks(fromPeer, Seq(block.header.number))(state).copy(
        readyBlocks = state.readyBlocks.enqueue(block),
        waitingHeaders = state.waitingHeaders.filter(block.header.number != _.number)
      )
    }

    def pickBlocks(amount: Int)(state: FetcherState): (Queue[Block], FetcherState) = {
      val (picked, rest) = state.readyBlocks.splitAt(amount)
      (picked, state.copy(readyBlocks = rest))
    }

    def invalidateBlocksFrom(nr: BigInt)(state: FetcherState): (Peer, FetcherState) =
      (
        state.blockProviders(nr),
        state.copy(
          readyBlocks = state.readyBlocks.takeWhile(_.header.number < nr),
          waitingHeaders = state.waitingHeaders.takeWhile(_.number < nr),
          lastBlock = nr - 1,
          headersFetcher = None,
          bodiesFetcher = None,
          blockProviders = Map()
        ))

    def withImporter(importer: ActorRef, state: FetcherState): FetcherState = state.copy(importer = Some(importer))

    def withLastBlock(nr: BigInt)(state: FetcherState): FetcherState = state.copy(lastBlock = nr)

    def withPeerForBlocks(peer: Peer, blocks: Seq[BigInt])(state: FetcherState): FetcherState =
      state.copy(
        blockProviders = state.blockProviders ++ blocks.map(block => block -> peer)
      )

    def withHeadersFetcher(fetcher: ActorRef)(state: FetcherState): FetcherState =
      state.copy(headersFetcher = Some(fetcher))

    def withoutHeadersFetcher(state: FetcherState): FetcherState = state.copy(headersFetcher = None)

    def withBodiesFetcher(fetcher: ActorRef)(state: FetcherState): FetcherState =
      state.copy(bodiesFetcher = Some(fetcher))

    def withoutBodiesFetcher(state: FetcherState): FetcherState = state.copy(bodiesFetcher = None)

    def withStateNodeFetcher(fetcher: StateNodeFetcher, state: FetcherState): FetcherState =
      state.copy(stateNodeFetcher = Some(fetcher))

    def withoutStateNodeFetcher(state: FetcherState): FetcherState = state.copy(stateNodeFetcher = None)

    def hasFetchedTop(state: FetcherState): FetcherState = state.copy(fetchedTop = true)

    def hasNotFetchedTop(state: FetcherState): FetcherState = state.copy(fetchedTop = false)

    def status(state: FetcherState): Map[String, Any] = Map(
      "ready blocks" -> state.readyBlocks.size,
      "fetched headers" -> state.waitingHeaders.size,
      "fetching headers" -> state.headersFetcher.fold("no")(actor => s"yes, from $actor"),
      "fetching bodies" -> state.bodiesFetcher.fold("no")(actor => s"yes, from $actor"),
      "fetching state node" -> state.stateNodeFetcher.fold("no")(fetcher => s"yes, from ${fetcher.theFetcher}"),
      "is on top" -> state.fetchedTop,
      "last block" -> state.lastBlock,
    )
  }

  implicit class HeadersSeqOps(val headers: Seq[BlockHeader]) extends AnyVal {
    def lastNumber: Option[BigInt] = headers.lastOption.map(_.number)
  }

  implicit val actorEq: Eq[ActorRef] = _ equals _
}
