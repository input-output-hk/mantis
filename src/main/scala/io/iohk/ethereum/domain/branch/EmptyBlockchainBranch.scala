package io.iohk.ethereum.domain.branch
import akka.util.ByteString

import io.iohk.ethereum.domain.Block

object EmptyBlockchainBranch extends BlockchainBranch {

  override def getBlockByNumber(number: BigInt): Option[Block] = None

  override def getHashByBlockNumber(number: BigInt): Option[ByteString] = None
}
