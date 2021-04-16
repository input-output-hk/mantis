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

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// scalastyle:off number.of.methods
/**
  * State used by the BlockFetcher
  *
  * @param readyBlocks
  * @param waitingHeaders
  * @param stateNodeFetcher
  * @param lastBlock
  * @param blockProviders
  */
case class BlockFetcherState(
    blockValidator: BlockValidator,
    readyBlocks: Queue[Block],
    waitingHeaders: Queue[BlockHeader],
    stateNodeFetcher: Option[StateNodeFetcher],
    lastBlock: BigInt,
    blockProviders: Map[BigInt, PeerId]
) {

  def isFetchingStateNode: Boolean = stateNodeFetcher.isDefined

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
    validatedHeaders(headers.sortBy(_.number)).map(validHeaders =>
        copy(
          waitingHeaders = waitingHeaders ++ validHeaders
        ))

  /**
    * Validates received headers consistency and their compatibility with the state
    * TODO ETCM-370: This needs to be more fine-grained and detailed so blacklisting can be re-enabled
    */
  private def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] =
    if (headers.isEmpty) {
      Right(headers)
    } else {
      headers
        .asRight[String]
        .ensure("Given headers should form a sequence without gaps")(HeadersSeq.areChain)
        .ensure("Given headers should form a sequence with ready blocks")(checkConsistencyWithReadyBlocks)
        .ensure("Given headers do not form a chain with already stored ones")(headers =>
          (waitingHeaders.lastOption, headers.headOption).mapN(_ isParentOf _).getOrElse(true)
        )
    }

  private def checkConsistencyWithReadyBlocks(headers: Seq[BlockHeader]): Boolean = {
    (readyBlocks, headers) match {
      case (_ :+ last, head +: _) if waitingHeaders.isEmpty => last.header isParentOf head
      case _ => true
    }
  }

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
        if (waitingHeader.hash == block.hash) {
          enqueueReadyBlock(block, fromPeer)
            .copy(
              waitingHeaders = waitingHeadersTail
            )
        } else
          this
      }
      .getOrElse(this)

  def enqueueReadyBlock(block: Block, fromPeer: PeerId): BlockFetcherState =
    withPeerForBlocks(fromPeer, Seq(block.number))
      .copy(readyBlocks = readyBlocks.enqueue(block))

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
    //TODO: update thee last block here?
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
    copy(
      readyBlocks = Queue(),
      waitingHeaders = Queue()
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

  def exists(hash: ByteString): Boolean = existsInReadyBlocks(hash) || existsInWaitingHeaders(hash)

  def existsInWaitingHeaders(hash: ByteString): Boolean = waitingHeaders.exists(_.hash == hash)

  def existsInReadyBlocks(hash: ByteString): Boolean = readyBlocks.exists(_.hash == hash)

  def withLastBlock(nr: BigInt): BlockFetcherState = copy(lastBlock = nr)

  def withUpdatedReadyBlocks(nr: BigInt): BlockFetcherState = copy(
    waitingHeaders = Queue(),
    readyBlocks = readyBlocks.dropWhile(_.number > nr)
  )

  def withPeerForBlocks(peerId: PeerId, blocks: Seq[BigInt]): BlockFetcherState =
    copy(blockProviders = blockProviders ++ blocks.map(block => block -> peerId))

  def fetchingStateNode(hash: ByteString, requestor: ActorRef): BlockFetcherState =
    copy(stateNodeFetcher = Some(StateNodeFetcher(hash, requestor)))

  def notFetchingStateNode(): BlockFetcherState = copy(stateNodeFetcher = None)

  def status: Map[String, Any] = Map(
    "ready blocks" -> readyBlocks.size,
  )

  def statusDetailed: Map[String, Any] = Map(
    "fetched headers" -> waitingHeaders.size,
    "fetching state node" -> isFetchingStateNode,
    "first header" -> waitingHeaders.headOption.map(_.number),
    "first block" -> readyBlocks.headOption.map(_.number),
    "last block" -> lastBlock
  )
}

object BlockFetcherState {
  case class StateNodeFetcher(hash: ByteString, replyTo: ActorRef)

  def initial(blockValidator: BlockValidator, lastBlock: BigInt): BlockFetcherState =
    BlockFetcherState(
      blockValidator = blockValidator,
      readyBlocks = Queue(),
      waitingHeaders = Queue(),
      stateNodeFetcher = None,
      lastBlock = lastBlock,
      blockProviders = Map()
    )
}
