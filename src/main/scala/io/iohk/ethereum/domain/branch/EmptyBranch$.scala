package io.iohk.ethereum.domain.branch
import akka.util.ByteString

object EmptyBranch extends Branch {

  override def isInChain(hash: ByteString): Boolean = false
}
