package io.iohk.ethereum.network

import akka.actor.{ActorRef, Actor}
import io.iohk.ethereum.network.FastSyncActor.HashType
import io.iohk.ethereum.utils.Config

class FastSyncNodesRequestHandler(peer: ActorRef, nodesHashes: Seq[HashType]) extends Actor {

  import context.system
  import Config.FastSync._
  import FastSyncNodesRequestHandler._

  val fastSyncController = context.parent

  system.scheduler.scheduleOnce(peerResponseTimeout, self, Timeout)

  override def receive = {
    
  }

}

object FastSyncNodesRequestHandler {
  private case object Timeout
}
