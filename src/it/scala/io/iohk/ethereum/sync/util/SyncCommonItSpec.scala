package io.iohk.ethereum.sync.util

import java.net.InetSocketAddress
import java.net.ServerSocket

import scala.util.Using

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

object SyncCommonItSpec {
  val IdentityUpdate: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = (_, world) => world

  def randomAddress(): InetSocketAddress =
    Using(new ServerSocket(0)) { s =>
      s.setReuseAddress(true) // read the javadoc and don't remove this line
      try new InetSocketAddress("localhost", s.getLocalPort)
    }.get

  final case class BlockchainState(
      bestBlock: Block,
      currentWorldState: InMemoryWorldStateProxy,
      currentWeight: ChainWeight
  )
}
