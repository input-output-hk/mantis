package io.iohk.ethereum.network

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.Config.Blockchain._

trait ForkResolver {
  type Fork <: ForkResolver.Fork

  def forkBlockNumber: BigInt
  def recognizeFork(blockHeader: BlockHeader): Fork
  def isAccepted(fork: Fork): Boolean
}

object ForkResolver {

  trait Fork

  object EtcForkResolver extends ForkResolver {
    sealed trait Fork extends ForkResolver.Fork
    case object Etc extends Fork
    case object Eth extends Fork

    override def forkBlockNumber: BigInt = daoForkBlockNumber

    override def recognizeFork(blockHeader: BlockHeader): Fork = {
      if (blockHeader.hash == daoForkBlockHash) Etc
      else Eth
    }

    override def isAccepted(fork: Fork): Boolean = fork == Etc
  }

}
