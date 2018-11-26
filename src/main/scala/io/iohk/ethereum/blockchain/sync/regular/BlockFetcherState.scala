package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.domain.{Block, BlockHeader, HeadersSeq}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHash}
import BlockFetcherState._
import cats.syntax.either._
import cats.syntax.option._

import scala.collection.immutable.Queue
case class BlockFetcherState(
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

  def hasReachedSize(size: Int): Boolean = (readyBlocks.size + waitingHeaders.size) >= size

  def lastFullBlockNumber: BigInt =
    readyBlocks.lastOption
      .map(Block.number)
      .orElse(waitingHeaders.headOption.map(_.number - 1))
      .getOrElse(lastBlock)

  def lowestBlock: BigInt =
    readyBlocks.headOption
      .map(Block.number)
      .orElse(waitingHeaders.headOption.map(_.number))
      .getOrElse(lastBlock)

  def nextToLastBlock: BigInt = lastBlock + 1

  def takeHashes(amount: Int): Seq[ByteString] = waitingHeaders.take(amount).map(_.hash)

  def appendHeaders(headers: Seq[BlockHeader]): BlockFetcherState =
    fetchingHeaders(false)
      .withPossibleNewTopAt(headers.lastOption.map(_.number))
      .copy(
        waitingHeaders = waitingHeaders ++ headers.filter(_.number > lastBlock).sortBy(_.number),
        lastBlock = HeadersSeq.lastNumber(headers).getOrElse(lastBlock)
      )

  def validatedHeaders(headers: Seq[BlockHeader]): Either[String, Seq[BlockHeader]] =
    if (headers.isEmpty) {
      Right(headers)
    } else {
      headers
        .asRight[String]
        .ensure("Given headers are not sequence with already fetched ones")(_.head.number <= nextToLastBlock)
        .ensure("Given headers aren't better than already fetched ones")(_.last.number > lastBlock)
        .ensure("Given headers should form a sequence without gaps")(HeadersSeq.areChain)
    }

  def validatedHashes(hashes: Seq[BlockHash]): Either[String, Seq[BlockHash]] =
    hashes
      .asRight[String]
      .ensure("Hashes are empty")(_.nonEmpty)
      .ensure("Hashes are too new")(_.head.number == nextToLastBlock)
      .ensure("Hashes should form a chain")(hashes =>
        hashes.zip(hashes.tail).forall {
          case (a, b) => a.number + 1 == b.number
      })

  def addBodies(peer: Peer, bodies: Seq[BlockBody]): BlockFetcherState = {
    val (matching, waiting) = waitingHeaders.splitAt(bodies.length)
    val blocks = matching.zip(bodies).map((Block.apply _).tupled)

    fetchingBodies(false)
      .withPeerForBlocks(peer.id, blocks.map(_.header.number))
      .copy(readyBlocks = readyBlocks.enqueue(blocks), waitingHeaders = waiting)
  }

  def appendNewBlock(block: Block, fromPeer: PeerId): BlockFetcherState =
    withPeerForBlocks(fromPeer, Seq(block.header.number))
      .withPossibleNewTopAt(Block.number(block))
      .copy(
        readyBlocks = readyBlocks.enqueue(block),
        waitingHeaders = waitingHeaders.filter(block.header.number != _.number)
      )

  def pickBlocks(amount: Int): Option[(NonEmptyList[Block], BlockFetcherState)] =
    if (readyBlocks.nonEmpty) {
      val (picked, rest) = readyBlocks.splitAt(amount)
      Some((NonEmptyList(picked.head, picked.tail.toList), copy(readyBlocks = rest)))
    } else {
      None
    }

  def strictPickBlocks(from: BigInt, atLeastWith: BigInt): Option[(NonEmptyList[Block], BlockFetcherState)] = {
    val lower = from.min(atLeastWith)
    val upper = from.max(atLeastWith)
    readyBlocks.some
      .filter(_.headOption.exists(block => Block.number(block) <= lower))
      .filter(_.lastOption.exists(block => Block.number(block) >= upper))
      .filter(_.nonEmpty)
      .map(blocks => (NonEmptyList(blocks.head, blocks.tail.toList), copy(readyBlocks = Queue())))
  }

  def invalidateBlocksFrom(nr: BigInt): (Option[PeerId], BlockFetcherState) = invalidateBlocksFrom(nr, Some(nr))

  def invalidateBlocksFrom(nr: BigInt, toBlacklist: Option[BigInt]): (Option[PeerId], BlockFetcherState) =
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

  def fetchingHeaders(isFetching: Boolean): BlockFetcherState = copy(isFetchingHeaders = isFetching)

  def fetchingBodies(isFetching: Boolean): BlockFetcherState = copy(isFetchingBodies = isFetching)

  def fetchingStateNode(hash: ByteString, requestor: ActorRef): BlockFetcherState =
    copy(stateNodeFetcher = Some(StateNodeFetcher(hash, requestor)))

  def notFetchingStateNode(): BlockFetcherState = copy(stateNodeFetcher = None)

  def status: Map[String, Any] = Map(
    "ready blocks" -> readyBlocks.size,
    "fetched headers" -> waitingHeaders.size,
    "fetching headers" -> isFetchingHeaders,
    "fetching bodies" -> isFetchingBodies,
    "fetching state node" -> isFetchingStateNode,
    "fetched top header" -> hasFetchedTopHeader,
    "first header" -> waitingHeaders.headOption.map(_.number),
    "first block" -> readyBlocks.headOption.map(Block.number),
    "last block" -> lastBlock,
    "known top" -> knownTop,
    "is on top" -> isOnTop
  )
}

object BlockFetcherState {
  case class StateNodeFetcher(hash: ByteString, replyTo: ActorRef)

  def initial(importer: ActorRef, lastBlock: BigInt): BlockFetcherState = BlockFetcherState(
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
