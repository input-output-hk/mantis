package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.implicits._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcherState._
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, HeadersSeq}
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import io.iohk.ethereum.utils.ByteStringUtils

import scala.annotation.tailrec
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
    blockValidator: BlockValidator,
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

  def hasFetchedTopHeader: Boolean = nextBlockToFetch == knownTop + 1

  def isOnTop: Boolean = hasFetchedTopHeader && hasEmptyBuffer

  def hasReachedSize(size: Int): Boolean = (readyBlocks.size + waitingHeaders.size) >= size

  def lowestBlock: BigInt =
    readyBlocks.headOption
      .map(_.number)
      .orElse(waitingHeaders.headOption.map(_.number))
      .getOrElse(lastBlock)

  /**
    * Next block number to be fetched, calculated in a way to maintain local queues consistency,
    * even if `lastBlock` property is much higher - it's more important to have this consistency
    * here and allow standard rollback/reorganization mechanisms to kick in if we get too far with mining,
    * therefore `lastBlock` is used here only if blocks and headers queues are empty
    */
  def nextBlockToFetch: BigInt = waitingHeaders.lastOption
    .map(_.number)
    .orElse(readyBlocks.lastOption.map(_.number))
    .getOrElse(lastBlock) + 1

  def takeHashes(amount: Int): Seq[ByteString] = waitingHeaders.take(amount).map(_.hash)

  def appendHeaders(headers: Seq[BlockHeader]): Either[String, BlockFetcherState] =
    validatedHeaders(headers.sortBy(_.number)).map(validHeaders => {
      val lastNumber = HeadersSeq.lastNumber(validHeaders)
      withPossibleNewTopAt(lastNumber)
        .copy(
          waitingHeaders = waitingHeaders ++ validHeaders
        )
    })

  def tryInsertBlock(block: Block, peerId: PeerId): Either[String, BlockFetcherState] = {
    val blockHash = block.hash
    if (isExist(blockHash)) {
      Right(this)
    } else if (isExistInReadyBlocks(block.header.parentHash)) {
      val newState = clearQueues()
        .copy(
          readyBlocks = readyBlocks.takeWhile(_.number < block.number).enqueue(block)
        )
        .withPeerForBlocks(peerId, Seq(block.number))
        .withKnownTopAt(block.number)
      Right(newState)
    } else if (isExistInWaitingHeaders(block.header.parentHash)) {
      // ignore already requested bodies
      val newFetchingBodiesState =
        if (fetchingBodiesState == AwaitingBodies) AwaitingBodiesToBeIgnored else fetchingBodiesState
      val newState = copy(
        waitingHeaders = waitingHeaders.takeWhile(_.number < block.number).enqueue(block.header),
        fetchingBodiesState = newFetchingBodiesState
      )
        .withKnownTopAt(block.number)
      Right(newState)
    } else Left(s"Cannot insert block [${ByteStringUtils.hash2string(blockHash)}] into the queues")
  }

  /**
    * Validates received headers consistency and their compatibilty with the state
    * TODO ETCM-370: This needs to be more fine-grained and detailed so blacklisting can be re-enabled
    */
  private def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] =
    if (headers.isEmpty) {
      Right(headers)
    } else {
      headers
        .asRight[String]
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

  /**
    * When bodies are requested, the response don't need to be a complete sub chain,
    * even more, we could receive an empty chain and that will be considered valid. Here we just
    * validate that the received bodies corresponds to an ordered subset of the requested headers.
    */
  def validateBodies(receivedBodies: Seq[BlockBody]): Either[String, Seq[Block]] =
    bodiesAreOrderedSubsetOfRequested(waitingHeaders.toList, receivedBodies)
      .toRight(
        "Received unrequested bodies"
      )

  // Checks that the received block bodies are an ordered subset of the ones requested
  @tailrec
  private def bodiesAreOrderedSubsetOfRequested(
      requestedHeaders: Seq[BlockHeader],
      respondedBodies: Seq[BlockBody],
      matchedBlocks: Seq[Block] = Nil
  ): Option[Seq[Block]] =
    (requestedHeaders, respondedBodies) match {
      case (Seq(), _ +: _) => None
      case (_, Seq()) => Some(matchedBlocks)
      case (header +: remainingHeaders, body +: remainingBodies) =>
        val doMatch = blockValidator.validateHeaderAndBody(header, body).isRight
        if (doMatch)
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, remainingBodies, matchedBlocks :+ Block(header, body))
        else
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, respondedBodies, matchedBlocks)
    }

  /**
    * If blocks is empty collection - headers in queue are removed as the cause is:
    *   - the headers are from rejected fork and therefore it won't be possible to resolve blocks for them
    *   - given peer is still syncing (quite unlikely due to preference of peers with best total difficulty
    *     when making a request)
    */
  def handleRequestedBlocks(blocks: Seq[Block], fromPeer: PeerId): BlockFetcherState =
    if (blocks.isEmpty)
      copy(
        waitingHeaders = Queue.empty
      )
    else
      blocks.foldLeft(this) { case (state, block) =>
        state.enqueueRequestedBlock(block, fromPeer)
      }

  /**
    * If the requested block is not the next in the line in the waiting headers queue,
    * we opt for not adding it in the ready blocks queue.
    */
  def enqueueRequestedBlock(block: Block, fromPeer: PeerId): BlockFetcherState =
    waitingHeaders.dequeueOption
      .map { case (waitingHeader, waitingHeadersTail) =>
        if (waitingHeader.hash == block.hash)
          withPeerForBlocks(fromPeer, Seq(block.number))
            .withPossibleNewTopAt(block.number)
            .copy(
              readyBlocks = readyBlocks.enqueue(block),
              waitingHeaders = waitingHeadersTail
            )
        else
          this
      }
      .getOrElse(this)

  def pickBlocks(amount: Int): Option[(NonEmptyList[Block], BlockFetcherState)] =
    if (readyBlocks.nonEmpty) {
      val (picked, rest) = readyBlocks.splitAt(amount)
      Some((NonEmptyList(picked.head, picked.tail.toList), copy(readyBlocks = rest, lastBlock = picked.last.number)))
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

  def clearQueues(): BlockFetcherState = {
    // We can't start completely from scratch as requests could be in progress, we have to keep special track of them
    val newFetchingHeadersState =
      if (fetchingHeadersState == AwaitingHeaders) AwaitingHeadersToBeIgnored else fetchingHeadersState
    val newFetchingBodiesState =
      if (fetchingBodiesState == AwaitingBodies) AwaitingBodiesToBeIgnored else fetchingBodiesState

    copy(
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      fetchingHeadersState = newFetchingHeadersState,
      fetchingBodiesState = newFetchingBodiesState
    )
  }

  def invalidateBlocksFrom(nr: BigInt): (Option[PeerId], BlockFetcherState) = invalidateBlocksFrom(nr, Some(nr))

  def invalidateBlocksFrom(nr: BigInt, toBlacklist: Option[BigInt]): (Option[PeerId], BlockFetcherState) = {
    (
      toBlacklist.flatMap(blockProviders.get),
      this
        .clearQueues()
        .copy(
          lastBlock = (nr - 2).max(0),
          blockProviders = blockProviders - nr
        )
    )
  }

  def isExist(hash: ByteString): Boolean = isExistInReadyBlocks(hash) || isExistInWaitingHeaders(hash)

  def isExistInWaitingHeaders(hash: ByteString): Boolean = waitingHeaders.exists(_.hash == hash)

  def isExistInReadyBlocks(hash: ByteString): Boolean = readyBlocks.exists(_.hash == hash)

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

  def initial(importer: ActorRef, blockValidator: BlockValidator, lastBlock: BigInt): BlockFetcherState =
    BlockFetcherState(
      importer = importer,
      blockValidator = blockValidator,
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
