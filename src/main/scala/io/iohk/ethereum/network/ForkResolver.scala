package io.iohk.ethereum.network

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.DaoForkConfig

trait ForkResolver {
  type Fork <: ForkResolver.Fork

  def forkBlockNumber: BigInt
  def recognizeFork(blockHeader: BlockHeader): Fork
  def isAccepted(fork: Fork): Boolean
}

object ForkResolver {

  trait Fork

  class EtcForkResolver(daoForkConfig: DaoForkConfig) extends ForkResolver {
    sealed trait Fork extends ForkResolver.Fork
    case object AcceptedFork extends Fork
    case object RejectedFork extends Fork

    override def forkBlockNumber: BigInt = daoForkConfig.forkBlockNumber

    override def recognizeFork(blockHeader: BlockHeader): Fork =
      if (blockHeader.hash == daoForkConfig.forkBlockHash) AcceptedFork
      else RejectedFork

    override def isAccepted(fork: Fork): Boolean = fork == AcceptedFork
  }

}
