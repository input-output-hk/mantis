package io.iohk.ethereum.sync.util

import java.net.InetSocketAddress
import java.net.ServerSocket

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

object SyncCommonItSpec {
  val IdentityUpdate: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = (_, world) => world

  def randomAddress(): InetSocketAddress = {
    val s = new ServerSocket(0)
    try new InetSocketAddress("localhost", s.getLocalPort)
    finally s.close()
  }

  final case class BlockchainState(
      bestBlock: Block,
      currentWorldState: InMemoryWorldStateProxy,
      currentWeight: ChainWeight
  )
}
