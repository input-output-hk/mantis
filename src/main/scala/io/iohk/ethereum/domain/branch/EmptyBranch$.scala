package io.iohk.ethereum.domain.branch
import akka.util.ByteString

import io.iohk.ethereum.domain.Block

object EmptyBranch extends Branch {

  override def getHashByBlockNumber(number: BigInt): Option[ByteString] = None

  override def isInChain(hash: ByteString): Boolean = false
}
