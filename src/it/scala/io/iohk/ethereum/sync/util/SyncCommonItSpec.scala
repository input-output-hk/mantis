package io.iohk.ethereum.sync.util

import java.net.{InetSocketAddress, ServerSocket}

import io.iohk.ethereum.domain.{Block, ChainWeight}
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
