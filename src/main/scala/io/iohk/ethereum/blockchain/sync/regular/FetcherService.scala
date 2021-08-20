package io.iohk.ethereum.blockchain.sync.regular

import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.util.ByteString

import cats.data.EitherT
import cats.implicits._

import monix.eval.Task

import scala.annotation.tailrec

import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.RequestFailed
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockBodies
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.utils.Config.SyncConfig

//not used atm, a part of the future ExecutionSync
class FetcherService(validator: BlockValidator, blockchainReader: BlockchainReader, syncConfig: SyncConfig) {

  val batchSize = syncConfig.blockHeadersPerRequest

  private def requestHeaders(
      block: Either[BigInt, ByteString],
      amount: BigInt
  ): Task[Either[RequestFailed, BlockHeaders]] = ???

  private def requestBodies(hashes: Seq[ByteString]): Task[Either[RequestFailed, BlockBodies]] = ???

//TODO: add private def requestStateNode(hash: ByteString): Task[Either[RequestFailed, Seq[ByteString]]] = ???

  private def placeBlockInPeerStream(block: Block, peer: Peer): Peer = ???

  def fetchBlocksUntil(
      peer: Peer,
      startFromBlock: Either[BigInt, ByteString],
      fetchUntilBlock: Either[BigInt, ByteString]
  ): EitherT[Task, RequestFailed, Unit] = {
    val endNumber: Option[BigInt] =
      fetchUntilBlock.fold(Some(_), blockchainReader.getBlockHeaderByHash(_).map(_.number))
    val startNumber: Option[BigInt] =
      startFromBlock.fold(Some(_), blockchainReader.getBlockHeaderByHash(_).map(_.number))

    //TODO: make sure negatives are not possible
    val startBatchBlocks: Option[Seq[BigInt]] =
      for {
        start <- startNumber
        end <- endNumber
      } yield start.to(end, batchSize)

    startBatchBlocks match {
      case None      => EitherT.leftT(RequestFailed(peer, "Couldn't find blocks to fetch"))
      case Some(seq) => seq.traverse(num => fetchBlocks(peer, batchSize, Left(num))).map(_ => ())
    }
  }

  private def fetchBlocks(
      peer: Peer,
      amount: BigInt,
      block: Either[BigInt, ByteString]
  ): EitherT[Task, RequestFailed, Peer] =
    for {
      headers <- EitherT(requestHeaders(block, amount))
      bodies <- EitherT(requestBodies(headers.headers.map(_.hash)))
      blocks <- EitherT.fromOption[Task](
        bodiesAreOrderedSubsetOfRequested(headers.headers, bodies.bodies),
        RequestFailed(peer, "Unmatching bodies")
      )
      _ = blocks.foreach(placeBlockInPeerStream(_, peer))
    } yield peer

  // Checks that the received block bodies are an ordered subset of the ones requested
  @tailrec
  private def bodiesAreOrderedSubsetOfRequested(
      requestedHeaders: Seq[BlockHeader],
      respondedBodies: Seq[BlockBody],
      matchedBlocks: Seq[Block] = Nil
  ): Option[Seq[Block]] =
    (requestedHeaders, respondedBodies) match {
      case (Seq(), _ +: _) => None
      case (_, Seq())      => Some(matchedBlocks)
      case (header +: remainingHeaders, body +: remainingBodies) =>
        if (validator.validateHeaderAndBody(header, body).isRight)
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, remainingBodies, matchedBlocks :+ Block(header, body))
        else
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, respondedBodies, matchedBlocks)
    }

}

object FetcherService {
  type Hashes = Seq[ByteString]

  type BlockIdentifier = (ByteString, ByteString)

  def blockIdentifier(blockHeader: BlockHeader): BlockIdentifier =
    (blockHeader.transactionsRoot, blockHeader.ommersHash)

  def blockIdentifier(blockBody: BlockBody): BlockIdentifier =
    (
      ByteString(StdBlockValidator.transactionsRootHash(blockBody).toIterable),
      ByteString(StdBlockValidator.blockBodyOmmersHash(blockBody).toIterable)
    )

  def buildBlocks(headers: Seq[BlockHeader], bodies: Seq[BlockBody]): Seq[Block] = {
    val bodyById = bodies.view.map(body => blockIdentifier(body) -> body).toMap
    for {
      header <- headers
      body <- bodyById.get(blockIdentifier(header))
    } yield Block(header, body)
  }

  /** State of block fetching stream after processing a given incoming message with block headers or bodies
    *
    * @param outstanding headers that are yet to be matched to bodies
    * @param bodiesRequest information for requesting bodies corresponding to newly outstanding headers
    * @param result blocks produced by matching received headers with received bodies
    */
  case class FetchState(
      outstanding: Set[BlockHeader],
      bodiesRequest: Option[(PeerId, Hashes)],
      result: Seq[Block]
  )
  object FetchState {
    val initial: FetchState = FetchState(Set.empty, None, Nil)
  }

  def fetchBlocksForHeaders[M](bodyRequestor: Sink[(PeerId, Hashes), M]): Flow[MessageFromPeer, Seq[Block], M] =
    Flow[MessageFromPeer]
      .scan(FetchState.initial) {
        case (FetchState(outstanding, _, _), MessageFromPeer(BlockHeaders(headers), peerId)) =>
          FetchState(outstanding.concat(headers), Some(peerId -> headers.map(_.hash)), Nil)
        case (FetchState(outstanding, _, _), MessageFromPeer(BlockBodies(bodies), _)) =>
          val blocks = buildBlocks(outstanding.toSeq, bodies)
          FetchState(outstanding.removedAll(blocks.map(_.header)), None, blocks)
      }
      .alsoToMat(
        Flow[FetchState]
          .collect { case FetchState(_, Some(bodiesRequest), _) => bodiesRequest }
          .toMat(bodyRequestor)(Keep.right)
      )(Keep.right)
      .collect { case FetchState(_, _, blocks) if blocks.nonEmpty => blocks }
}
