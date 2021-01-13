package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.domain.BlockHeader

trait HeaderSkeleton {

  def firstSkeletonNumber: BigInt
  def gapSize: Int
  def blockHeadersPerRequest: Int
  def setSkeletonHeaders(headers: Seq[BlockHeader]): Option[HeaderSkeleton]

  def batchStartingHeaders: Seq[BigInt]
  def batchSize: Int
  def addBatch(headers: Seq[BlockHeader]): Option[HeaderSkeleton]

  def completeChain: Option[Seq[BlockHeader]]
}
