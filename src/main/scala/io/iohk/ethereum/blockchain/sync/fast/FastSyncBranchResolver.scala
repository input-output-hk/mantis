package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.Actor
import io.iohk.ethereum.network.Peer

class FastSyncBranchResolver extends Actor {

  override def receive: Receive = ???

}

object FastSyncBranchResolver {
  sealed trait BranchResolverRequest
  case object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  case class BranchResolvedSuccessful(firstCommonBlockNumber: BigInt, masterPeer: Peer) extends BranchResolverResponse
}
