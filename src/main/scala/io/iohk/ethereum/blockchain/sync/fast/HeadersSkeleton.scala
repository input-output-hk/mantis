package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.domain.BlockHeader

case class HeadersSkeleton(
    bestBlockHeaderNumber: BigInt,
    safeDownloadTarget: BigInt,
    headersPerRequest: Int,
    gapSize: Int) {

  lazy val start: BigInt = bestBlockHeaderNumber + 1

  private lazy val remainingBlockHeaders: BigInt = safeDownloadTarget - bestBlockHeaderNumber

  lazy val skip: BigInt = (remainingBlockHeaders - 2).min(gapSize)

  private lazy val intervalSize: BigInt = skip + 1

  lazy val limit: BigInt = {
    val remainingSkeletonHeaders = remainingBlockHeaders / intervalSize + (remainingBlockHeaders % intervalSize).min(1)
    remainingSkeletonHeaders.min(headersPerRequest)
  }

  private lazy val last: BigInt = start + intervalSize * (limit - 1)

  private lazy val toSeq: Seq[BigInt] = start to last by intervalSize

  def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    headers.zip(toSeq).forall { case (header, number) => header.number == number }

  def gapBlockNumbers: Seq[BigInt] = toSeq.zip(toSeq.drop(1)).flatMap { case (a, b) => a + 1 until b }
}
