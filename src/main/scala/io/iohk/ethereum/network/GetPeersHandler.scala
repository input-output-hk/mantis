package io.iohk.ethereum.network

import akka.actor.ActorRef
import io.iohk.ethereum.network.BlockBroadcastActor.ProcessingState
import io.iohk.ethereum.network.MaxBlockNumberRequestHandler.RequestMaxBlockNumber
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, PeersResponse}
import io.iohk.ethereum.network.GetPeersHandler.RequestPeers

trait GetPeersHandler { this: BlockBroadcastActor =>

  /**
    * This function handles the RequestPeers message by asking the PeerManager for the current peers.
    */
  def handleRequestPeers(state: ProcessingState, peerManager: ActorRef): Receive = {
    case RequestPeers => peerManager ! GetPeers
  }

  /**
    * This function handles the PeersResponse message by requesting the max known block number from each peer which will result
    * in sending the state.toBroadcastBlocks blocks if needed.
    */
  def handlePeersResponse(state: ProcessingState): Receive = {
    case PeersResponse(peers) if state.toBroadcastBlocks.nonEmpty =>
      self ! RequestMaxBlockNumber(peers.map(_.ref), state.toBroadcastBlocks)
      context become processMessages(state.withToBroadcastBlocks(Nil))
  }
}

object GetPeersHandler {
  case object RequestPeers
}
