package io.iohk.ethereum.network

import io.iohk.ethereum.daoFork.DaoForkConfig
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

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

    override def forkBlockNumber: BigInt = daoForkConfig.daoForkBlockNumber

    override def recognizeFork(blockHeader: BlockHeader): Fork = {
      if (blockHeader.hash == daoForkConfig.daoForkBlockHash) AcceptedFork
      else RejectedFork
    }

    override def isAccepted(fork: Fork): Boolean = fork == AcceptedFork
  }

}
