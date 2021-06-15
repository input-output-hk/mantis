package io.iohk.ethereum.forkid

import io.iohk.ethereum.utils.BlockchainConfig

object ForkId {

  val noFork = BigInt("1000000000000000000")

  def gatherForks(config: BlockchainConfig): List[BigInt] =
    (config.daoForkConfig.map(_.forkBlockNumber).toList ++
      config.forkBlockNumbers.productIterator.toList.flatMap {
        case i: BigInt => Some(i)
        case i: Option[_] =>
          i.flatMap {
            case n if n.isInstanceOf[BigInt] => Some(n.asInstanceOf[BigInt])
            case n => None
          }
        case default => None
      })
      .filterNot(v => v == 0 || v == noFork)
      .distinct
      .sorted

}
