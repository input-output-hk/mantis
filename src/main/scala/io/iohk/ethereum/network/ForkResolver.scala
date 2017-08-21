package io.iohk.ethereum.network

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

  class EtcForkResolver(blockchainConfig: BlockchainConfig) extends ForkResolver {
    sealed trait Fork extends ForkResolver.Fork
    case object AcceptedFork extends Fork
    case object RejectedFork extends Fork

    override def forkBlockNumber: BigInt = blockchainConfig.daoForkBlockNumber

    override def recognizeFork(blockHeader: BlockHeader): Fork = {
      if (blockHeader.hash == blockchainConfig.daoForkBlockHash) AcceptedFork
      else RejectedFork
    }

    override def isAccepted(fork: Fork): Boolean = fork == AcceptedFork
  }

}
