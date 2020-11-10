package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockBody, HeadersSeq}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import BlockFetcherState._
import cats.implicits._

import scala.collection.immutable.Queue

// scalastyle:off number.of.methods
/**
  * State used by the BlockFetcher
  *
  * @param importer the BlockImporter actor reference
  * @param readyBlocks
  * @param waitingHeaders
  * @param fetchingHeadersState the current state of the headers fetching, whether we
  *                             - haven't fetched any yet
  *                             - are awaiting a response
  *                             - are awaiting a response but it should be ignored due to blocks being invalidated
  * @param fetchingBodiesState the current state of the bodies fetching, whether we
  *                             - haven't fetched any yet
  *                             - are awaiting a response
  *                             - are awaiting a response but it should be ignored due to blocks being invalidated
  * @param stateNodeFetcher
  * @param lastBlock
  * @param knownTop
  * @param blockProviders
  */
case class BlockFetcherState(
    importer: ActorRef,
    readyBlocks: Queue[Block],
    waitingHeaders: Queue[BlockHeader],
    fetchingHeadersState: FetchingHeadersState,
    fetchingBodiesState: FetchingBodiesState,
    stateNodeFetcher: Option[StateNodeFetcher],
    lastBlock: BigInt,
    knownTop: BigInt,
    blockProviders: Map[BigInt, PeerId]
) {

  def isFetching: Boolean = isFetchingHeaders || isFetchingBodies

  def isFetchingStateNode: Boolean = stateNodeFetcher.isDefined

  private def hasEmptyBuffer: Boolean = readyBlocks.isEmpty && waitingHeaders.isEmpty

  def hasFetchedTopHeader: Boolean = lastBlock == knownTop

  def isOnTop: Boolean = hasFetchedTopHeader && hasEmptyBuffer

  def hasReachedSize(size: Int): Boolean = (readyBlocks.size + waitingHeaders.size) >= size

  def lastFullBlockNumber: BigInt =
    readyBlocks.lastOption
      .map(_.number)
      .orElse(waitingHeaders.headOption.map(_.number - 1))
      .getOrElse(lastBlock)

  def lowestBlock: BigInt =
    readyBlocks.headOption
      .map(_.number)
      .orElse(waitingHeaders.headOption.map(_.number))
      .getOrElse(lastBlock)

  def nextToLastBlock: BigInt = lastBlock + 1

  def takeHashes(amount: Int): Seq[ByteString] = waitingHeaders.take(amount).map(_.hash)

  def appendHeaders(headers: Seq[BlockHeader]): Either[String, BlockFetcherState] =
    validatedHeaders(headers.sortBy(_.number)).map(validHeaders => {
      val lastNumber = HeadersSeq.lastNumber(validHeaders)
      withPossibleNewTopAt(lastNumber)
        .copy(
          waitingHeaders = waitingHeaders ++ validHeaders,
          lastBlock = lastNumber.getOrElse(lastBlock)
        )
    })

  def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] =
    if (headers.isEmpty) {
      Right(headers)
    } else {
      headers
        .asRight[String]
        .ensure("Given headers are not sequence with already fetched ones")(_.head.number <= nextToLastBlock)
        .ensure("Given headers aren't better than already fetched ones")(_.last.number > lastBlock)
        .ensure("Given headers should form a sequence without gaps")(HeadersSeq.areChain)
        .ensure("Given headers do not form a chain with already stored ones")(headers =>
          (waitingHeaders.lastOption, headers.headOption).mapN(_ isParentOf _).getOrElse(true)
        )
    }

  def validateNewBlockHashes(hashes: Seq[BlockHash]): Either[String, Seq[BlockHash]] =
    hashes
      .asRight[String]
      .ensure("Hashes are empty")(_.nonEmpty)
      .ensure("Hashes should form a chain")(hashes =>
        hashes.zip(hashes.tail).forall { case (a, b) =>
          a.number + 1 == b.number
        }
      )

  def addBodies(peer: Peer, bodies: Seq[BlockBody]): BlockFetcherState = {
    val (matching, waiting) = waitingHeaders.splitAt(bodies.length)
    val blocks = matching.zip(bodies).map((Block.apply _).tupled)

    withPeerForBlocks(peer.id, blocks.map(_.header.number))
      .copy(
        readyBlocks = readyBlocks.enqueue(blocks),
        waitingHeaders = waiting
      )
  }

  def appendNewBlock(block: Block, fromPeer: PeerId): BlockFetcherState =
    withPeerForBlocks(fromPeer, Seq(block.header.number))
      .withPossibleNewTopAt(block.number)
      .withLastBlock(block.number)
      .copy(
        readyBlocks = readyBlocks.enqueue(block),
        waitingHeaders = waitingHeaders.filter(block.number != _.number)
      )

  def pickBlocks(amount: Int): Option[(NonEmptyList[Block], BlockFetcherState)] =
    if (readyBlocks.nonEmpty) {
      val (picked, rest) = readyBlocks.splitAt(amount)
      Some((NonEmptyList(picked.head, picked.tail.toList), copy(readyBlocks = rest)))
    } else {
      None
    }

  /**
    * Returns all the ready blocks but only if it includes blocks with number:
    * - lower = min(from, atLeastWith)
    * - upper = max(from, atLeastWith)
    */
  def strictPickBlocks(from: BigInt, atLeastWith: BigInt): Option[(NonEmptyList[Block], BlockFetcherState)] = {
    val lower = from.min(atLeastWith)
    val upper = from.max(atLeastWith)

    readyBlocks.some
      .filter(_.headOption.exists(block => block.number <= lower))
      .filter(_.lastOption.exists(block => block.number >= upper))
      .filter(_.nonEmpty)
      .map(blocks => (NonEmptyList(blocks.head, blocks.tail.toList), copy(readyBlocks = Queue())))
  }

  def invalidateBlocksFrom(nr: BigInt): (Option[PeerId], BlockFetcherState) = invalidateBlocksFrom(nr, Some(nr))

  def invalidateBlocksFrom(nr: BigInt, toBlacklist: Option[BigInt]): (Option[PeerId], BlockFetcherState) = {
    // We can't start completely from scratch as requests could be in progress, we have to keep special track of them
    val newFetchingHeadersState =
      if (fetchingHeadersState == AwaitingHeaders) AwaitingHeadersToBeIgnored else fetchingHeadersState
    val newFetchingBodiesState =
      if (fetchingBodiesState == AwaitingBodies) AwaitingBodiesToBeIgnored else fetchingBodiesState

    (
      toBlacklist.flatMap(blockProviders.get),
      copy(
        readyBlocks = Queue(),
        waitingHeaders = Queue(),
        lastBlock = (nr - 2).max(0),
        fetchingHeadersState = newFetchingHeadersState,
        fetchingBodiesState = newFetchingBodiesState,
        blockProviders = blockProviders - nr
      )
    )
  }

  def withLastBlock(nr: BigInt): BlockFetcherState = copy(lastBlock = nr)

  def withKnownTopAt(nr: BigInt): BlockFetcherState = copy(knownTop = nr)

  def withPossibleNewTopAt(nr: BigInt): BlockFetcherState =
    if (nr > knownTop) {
      withKnownTopAt(nr)
    } else {
      this
    }
  def withPossibleNewTopAt(nr: Option[BigInt]): BlockFetcherState = nr.map(withPossibleNewTopAt).getOrElse(this)

  def withPeerForBlocks(peerId: PeerId, blocks: Seq[BigInt]): BlockFetcherState =
    copy(blockProviders = blockProviders ++ blocks.map(block => block -> peerId))

  def isFetchingHeaders: Boolean = fetchingHeadersState != NotFetchingHeaders
  def withNewHeadersFetch: BlockFetcherState = copy(fetchingHeadersState = AwaitingHeaders)
  def withHeaderFetchReceived: BlockFetcherState = copy(fetchingHeadersState = NotFetchingHeaders)

  def isFetchingBodies: Boolean = fetchingBodiesState != NotFetchingBodies
  def withNewBodiesFetch: BlockFetcherState = copy(fetchingBodiesState = AwaitingBodies)
  def withBodiesFetchReceived: BlockFetcherState = copy(fetchingBodiesState = NotFetchingBodies)

  def fetchingStateNode(hash: ByteString, requestor: ActorRef): BlockFetcherState =
    copy(stateNodeFetcher = Some(StateNodeFetcher(hash, requestor)))

  def notFetchingStateNode(): BlockFetcherState = copy(stateNodeFetcher = None)

  def status: Map[String, Any] = Map(
    "ready blocks" -> readyBlocks.size,
    "known top" -> knownTop,
    "is on top" -> isOnTop
  )

  def statusDetailed: Map[String, Any] = Map(
    "fetched headers" -> waitingHeaders.size,
    "fetching headers" -> isFetchingHeaders,
    "fetching bodies" -> isFetchingBodies,
    "fetching state node" -> isFetchingStateNode,
    "fetched top header" -> hasFetchedTopHeader,
    "first header" -> waitingHeaders.headOption.map(_.number),
    "first block" -> readyBlocks.headOption.map(_.number),
    "last block" -> lastBlock
  )
}

object BlockFetcherState {
  case class StateNodeFetcher(hash: ByteString, replyTo: ActorRef)

  def initial(importer: ActorRef, lastBlock: BigInt): BlockFetcherState = BlockFetcherState(
    importer = importer,
    readyBlocks = Queue(),
    waitingHeaders = Queue(),
    fetchingHeadersState = NotFetchingHeaders,
    fetchingBodiesState = NotFetchingBodies,
    stateNodeFetcher = None,
    lastBlock = lastBlock,
    knownTop = lastBlock + 1,
    blockProviders = Map()
  )

  trait FetchingHeadersState
  case object NotFetchingHeaders extends FetchingHeadersState
  case object AwaitingHeaders extends FetchingHeadersState

  /**
    * Headers request in progress but will be ignored due to invalidation
    * State used to keep track of pending request to prevent multiple requests in parallel
    */
  case object AwaitingHeadersToBeIgnored extends FetchingHeadersState

  trait FetchingBodiesState
  case object NotFetchingBodies extends FetchingBodiesState
  case object AwaitingBodies extends FetchingBodiesState

  /**
    * Bodies request in progress but will be ignored due to invalidation
    * State used to keep track of pending request to prevent multiple requests in parallel
    */
  case object AwaitingBodiesToBeIgnored extends FetchingBodiesState
}
