package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.blockchain.sync.fast.HeaderSkeleton._
import io.iohk.ethereum.domain.BlockHeader

/**
  * This class contains the state of the current skeleton being downloaded. This state is represented as the downloaded
  * skeleton headers plus the downloaded batches.
  * A skeleton of block headers consists of `limit` headers, separated by `gapSize` blocks in between.
  * A batch of blocks is a sequence of `gapSize + 1` block headers starting one block after the previous skeleton
  * header up to the next skeleton header inclusive.
  * When a batch of headers is downloaded, it is checked against the current skeleton and if it is correct, we save it
  * into the state.
  * When all batches filling the gaps are downloaded, this skeleton is considered full and the `fullChain` can be
  * requested.
  *
  * Example:
  * Given from = 0, to = 10, maxSkeletonHeaders = 4
  * Then:
  * - firstSkeletonHeaderNumber = 3
  * - gapSize = 2
  * - batchSize = 3
  * - skeletonHeaderNumbers = Seq(3, 6, 9)
  * - batchStartingHeaderNumbers = Seq(0, 4, 7)
  *
  *                gap                           batch                     batch
  *           /----------\               /-------------------\      /------------------\
  *   0        1        2        3        4        5        6        7       8        9           10
  *   |                          |                          |                         |           |
  * from               1stSkeletonHeader          2ndSkeletonHeader       lastSkeletonHeader      to
  *
  * @param from Lower bound for this skeleton, inclusive
  * @param to Upper bound for this skeleton, inclusive
  * @param maxSkeletonHeaders Maximum number of skeleton headers
  * @param skeletonHeaders The currently downloaded skeleton headers. May be empty if none were downloaded. This is set
  *                        by using `setSkeletonHeaders`
  * @param batches The currently downloaded batches. This is filled in by using `addBatch`
  */
case class HeaderSkeleton(
    from: BigInt,
    to: BigInt,
    maxSkeletonHeaders: Int,
    private val skeletonHeaders: Seq[BlockHeader] = Seq.empty,
    private val batches: Map[BigInt, Seq[BlockHeader]] = Map.empty) {

  /**
    * Not to be confused with `from`. This is the number of the first header in the skeleton.
    */
  lazy val firstSkeletonHeaderNumber: BigInt = from + batchSize

  /**
    * Number of blocks in between each skeleton header
    */
  lazy val gapSize: BigInt = remainingBlocks.min(maxSkeletonHeaders) - 2


  private lazy val remainingBlocks: BigInt = to - from

  /**
    * Maximum number of blocks to be downloaded at once. This is the total number of blocks that the skeleton contains.
    */
  lazy val limit: BigInt = {
    val remainingSkeletonHeaders = remainingBlocks / batchSize + (remainingBlocks % batchSize).min(1)
    remainingSkeletonHeaders.min(maxSkeletonHeaders)
  }

  private lazy val lastSkeletonHeaderNumber: BigInt = from + batchSize * (limit - 1)
  private lazy val skeletonHeaderNumbers: Seq[BigInt] =
    firstSkeletonHeaderNumber to lastSkeletonHeaderNumber by batchSize

  /**
    * Use this method to update this state with the downloaded skeleton
    * @param headers The downloaded skeleton
    * @return Either the updated structure if the validation succeeded or an error
    */
  def setSkeletonHeaders(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, HeaderSkeleton] =
    for {
      _ <- checkSkeletonHeadersTotal(headers)
      _ <- checkSkeletonHeaderNumbers(headers)
    } yield copy(skeletonHeaders = headers)

  private def checkSkeletonHeadersTotal(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, Unit] =
    Either.cond(headers.size == limit, (), InvalidTotalHeaders(headers.size, limit.toInt))

  private def checkSkeletonHeaderNumbers(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, Unit] = {
    val downloadedHeaderNumbers = headers.map(_.number)
    val isValid = downloadedHeaderNumbers.zip(skeletonHeaderNumbers).forall {
      case (downloadedHeaderNumber, skeletonNumber) => downloadedHeaderNumber == skeletonNumber
    }
    Either.cond(isValid, (), InvalidHeaderNumber(downloadedHeaderNumbers, skeletonHeaderNumbers))
  }

  /**
    * An ordered sequence with the numbers of the first block of each batch
    */
  lazy val batchStartingHeaderNumbers: Seq[BigInt] = from +: skeletonHeaderNumbers.dropRight(1).map(_ + 1)

  /**
    * Number of batched headers to request to a peer
    */
  lazy val batchSize: BigInt = gapSize + 1

  /**
    * Use this method to update this state with a downloaded batch of headers
    * @param batchHeaders The downloaded batch of headers
    * @return Either the updated structure if the validation succeeded or an error
    */
  def addBatch(batchHeaders: Seq[BlockHeader]): Either[HeaderSkeletonError, HeaderSkeleton] =
    for {
      skeletonHeader <- findSkeletonHeader(batchHeaders)
      _ <- checkSkeletonParentHash(batchHeaders, skeletonHeader)
      batchStartingNumber <- findBatchStartingNumber(batchHeaders)
    } yield copy(batches = batches + (batchStartingNumber -> batchHeaders))

  private def findSkeletonHeader(batchHeaders: Seq[BlockHeader]): Either[HeaderBatchError, BlockHeader] = {
    batchHeaders.lastOption match {
      case Some(header) =>
        val maybeSkeleton = skeletonHeaders.find(s => s.number == header.number && s.hash == header.hash)
        maybeSkeleton.toRight(InvalidBatchLastNumber(header.number, skeletonHeaderNumbers))
      case None =>
        Left(EmptyDownloadedBatch(skeletonHeaderNumbers))
    }
  }

  private def checkSkeletonParentHash(
      batchHeaders: Seq[BlockHeader],
      skeletonHeader: BlockHeader
  ): Either[HeaderBatchError, Unit] = {
    batchHeaders.dropRight(1).lastOption match {
      case Some(penultimateBatchHeader) if penultimateBatchHeader.hash != skeletonHeader.hash =>
        Left(InvalidBatchChain(penultimateBatchHeader, skeletonHeader))
      case _ =>
        Right(())
    }
  }

  private def findBatchStartingNumber(batchHeaders: Seq[BlockHeader]): Either[HeaderBatchError, BigInt] = {
    batchHeaders.headOption.map(_.number) match {
      case Some(firstBatchHeader) =>
        val found = batchStartingHeaderNumbers.find(_ == firstBatchHeader)
        found.toRight(InvalidBatchFirstNumber(firstBatchHeader, batchStartingHeaderNumbers))
      case None =>
        Left(EmptyDownloadedBatch(skeletonHeaderNumbers))
    }
  }

  /**
    * The complete skeleton plus the filled in batches, or `None` if not everything was downloaded
    */
  lazy val fullChain: Option[Seq[BlockHeader]] =
    if (isFull) Some(batchStartingHeaderNumbers.flatMap(batches.apply))
    else None
  private lazy val isFull: Boolean = batchStartingHeaderNumbers.forall(batches.contains)
}

object HeaderSkeleton {

  sealed trait HeaderSkeletonError {
    def msg: String
  }
  case class InvalidTotalHeaders(downloaded: Int, expected: Int) extends HeaderSkeletonError {
    override def msg: String = s"Invalid downloaded total headers. Expected $expected but was $downloaded"
  }
  case class InvalidHeaderNumber(downloaded: Seq[BigInt], expected: Seq[BigInt]) extends HeaderSkeletonError {
    override def msg: String = s"Invalid sequence of skeleton headers. Expected $expected but was $downloaded"
  }

  sealed trait HeaderBatchError extends HeaderSkeletonError
  case class InvalidBatchLastNumber(downloaded: BigInt, expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Invalid batch last number. $downloaded wasn't found in $expected"
  }
  case class EmptyDownloadedBatch(expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Downloaded empty headers batch. Expected $expected"
  }
  case class InvalidBatchChain(penultimateBatchHeader: BlockHeader, skeletonHeader: BlockHeader) extends HeaderBatchError {
    override def msg: String = s"Invalid batch penultimate header. $penultimateBatchHeader isn't parent of $skeletonHeader"
  }
  case class InvalidBatchFirstNumber(downloaded: BigInt, expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Invalid batch first number. $downloaded wasn't found in $expected"
  }
}
