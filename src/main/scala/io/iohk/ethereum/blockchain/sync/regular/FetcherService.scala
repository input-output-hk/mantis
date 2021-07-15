package io.iohk.ethereum.blockchain.sync.regular

import akka.util.ByteString
import cats.data.EitherT
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, BlockchainReader}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.ETH62.{BlockBodies, BlockHeaders}
import monix.eval.Task
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.RequestFailed
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.utils.Config.SyncConfig
import cats.implicits._

import scala.annotation.tailrec

//not used atm, a part of the future ExecutionSync
class FetcherService(validator: BlockValidator, blockchainReader: BlockchainReader, syncConfig: SyncConfig) {
  val batchSize = syncConfig.blockHeadersPerRequest

  private def requestHeaders(
      block: Either[BigInt, ByteString],
      amount: BigInt
  ): Task[Either[RequestFailed, BlockHeaders]] = ???

  private def requestBodies(hashes: Seq[ByteString]): Task[Either[RequestFailed, BlockBodies]] = ???

  private def requestStateNode(hash: ByteString): Task[Either[RequestFailed, Seq[ByteString]]] = ???

  private def placeBlockInQueue(block: Block, peer: Peer): Unit = ???

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
      case None      => EitherT.leftT(RequestFailed(peer, "couldn't find blocks to fetch"))
      case Some(seq) => seq.traverse(num => fetchBlocks(peer, batchSize, Left(num))).map(_ => ())
    }
  }

  private def fetchBlocks(
      peer: Peer,
      amount: BigInt,
      block: Either[BigInt, ByteString]
  ): EitherT[Task, RequestFailed, Unit] =
    for {
      headers <- EitherT(requestHeaders(block, amount))
      bodies <- EitherT(requestBodies(headers.headers.map(_.hash)))
      blocks <- EitherT.fromOption[Task](
        bodiesAreOrderedSubsetOfRequested(headers.headers, bodies.bodies),
        RequestFailed(peer, "Unmatching bodies")
      )
      _ = blocks.foreach(placeBlockInQueue(_, peer))
    } yield ()

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
        val doMatch = validator.validateHeaderAndBody(header, body).isRight
        if (doMatch)
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, remainingBodies, matchedBlocks :+ Block(header, body))
        else
          bodiesAreOrderedSubsetOfRequested(remainingHeaders, respondedBodies, matchedBlocks)
    }
}
