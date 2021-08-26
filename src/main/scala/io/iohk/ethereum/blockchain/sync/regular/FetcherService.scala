package io.iohk.ethereum.blockchain.sync.regular

import akka.NotUsed
import akka.stream.QueueOfferResult.Enqueued
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.SourceQueue
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
class FetcherService(
    blockchainReader: BlockchainReader,
    syncConfig: SyncConfig,
    sourceQueue: SourceQueue[Block]
) {

  import FetcherService._

  val batchSize = syncConfig.blockHeadersPerRequest

  private def requestHeaders(
      block: Either[BigInt, ByteString],
      amount: BigInt
  ): Task[Either[RequestFailed, BlockHeaders]] = ???

  private def requestBodies(hashes: Seq[ByteString]): Task[Either[RequestFailed, BlockBodies]] = ???

//TODO: add private def requestStateNode(hash: ByteString): Task[Either[RequestFailed, Seq[ByteString]]] = ???

  def placeBlockInPeerStream(block: Block): Task[Either[String, Unit]] =
    Task.deferFuture(sourceQueue.offer(block)).map {
      case Enqueued => Right(())
      case reason   => Left(s"SourceQueue.offer failed: $reason")
    }

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
      blocks = buildBlocks(headers.headers, bodies.bodies)
      _ <- EitherT.cond[Task](blocks.length == headers.headers.length, (), RequestFailed(peer, "Unmatching bodies"))
      _ <- blocks.traverse(block => EitherT(placeBlockInPeerStream(block)).leftMap(RequestFailed(peer, _)))
    } yield peer
}

object FetcherService {
  case class BlockIdentifier(transactionsRoot: ByteString, ommersHash: ByteString)
  object BlockIdentifier {
    def apply(blockHeader: BlockHeader): BlockIdentifier =
      BlockIdentifier(blockHeader.transactionsRoot, blockHeader.ommersHash)

    def apply(blockBody: BlockBody): BlockIdentifier =
      BlockIdentifier(
        ByteString(StdBlockValidator.transactionsRootHash(blockBody).toIterable),
        ByteString(StdBlockValidator.blockBodyOmmersHash(blockBody).toIterable)
      )
  }

  def buildBlocks(headers: Seq[BlockHeader], bodies: Seq[BlockBody]): Seq[Block] = {
    val bodyById = bodies.view.map(body => BlockIdentifier(body) -> body).toMap
    for {
      header <- headers
      body <- bodyById.get(BlockIdentifier(header))
    } yield Block(header, body)
  }

  /** State of block fetching stream after processing a given incoming message with block headers or bodies
    *
    * @param outstanding headers that are yet to be matched to bodies
    * @param result blocks produced by matching received headers with received bodies
    */
  case class FetchState(
      outstanding: Set[BlockHeader],
      result: Seq[Block]
  )
  object FetchState {
    val initial: FetchState = FetchState(Set.empty, Nil)
  }

  // TODO: remove once we have the FetcherService instance integrated
  val tempFlow: Flow[MessageFromPeer, Seq[Block], NotUsed] =
    Flow[MessageFromPeer]
      .scan(FetchState.initial) {
        case (FetchState(outstanding, _), MessageFromPeer(BlockHeaders(headers), peerId)) =>
          FetchState(outstanding.concat(headers), Nil)
        case (FetchState(outstanding, _), MessageFromPeer(BlockBodies(bodies), _)) =>
          val blocks = buildBlocks(outstanding.toSeq, bodies)
          FetchState(outstanding.removedAll(blocks.map(_.header)), blocks)
      }
      .collect { case FetchState(_, blocks) if blocks.nonEmpty => blocks }
}
