package io.iohk.ethereum.daoFork

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

trait DaoForkConfig {

  val forkBlockNumber: BigInt
  val forkBlockHash: ByteString
  val blockExtraData: Option[ByteString]
  val range: Int
  val refundContract: Option[Address]
  val drainList: Seq[Address]

  private lazy val extratadaBlockRange = forkBlockNumber until(forkBlockNumber + range)

  def isDaoForkBlock(blockNumber: BigInt): Boolean = forkBlockNumber == blockNumber

  def requiresExtraData(blockNumber: BigInt): Boolean = blockExtraData.isDefined && (extratadaBlockRange contains blockNumber)

  def getExtraData(blockNumber: BigInt): Option[ByteString] =
    if(requiresExtraData(blockNumber)) blockExtraData
    else None
}
