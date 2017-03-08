package io.iohk.ethereum.network

import akka.actor.ActorRef
import io.iohk.ethereum.network.BlockBroadcastActor.ProcessingState
import io.iohk.ethereum.network.MaxBlockNumberRequestHandler.{PeerTerminated, RequestMaxBlockNumber}
import io.iohk.ethereum.network.PeerActor.{GetMaxBlockNumber, MaxBlockNumber}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock

//FIXME: Decide block propagation algorithm (for now we send block to every peer except the sender) [EC-87]
trait MaxBlockNumberRequestHandler { this: BlockBroadcastActor =>

  /**
    * This function handles the RequestMaxBlockNumber message:
    *   - It requests the MaxBlockNumber from each peer that didn't send the blocks
    *   - Modifies the state to keep the blocks that need to could sent to each peer (if they don't already know of them)
    */
  def handleRequestMaxBlockNumber(state: ProcessingState): Receive = {
    case RequestMaxBlockNumber(peers, blocksToSend) =>
      if(blocksToSend.nonEmpty) {
        val newState = peers.foldLeft(state) { case (recState, peerToSend) =>
          requestMaxBlockNumberToEachPeer(recState, peerToSend, blocksToSend)
        }
        context become processMessages(newState)
      }
  }

  private def requestMaxBlockNumberToEachPeer(state: ProcessingState, peerToSend: ActorRef, blocksToSend: Seq[NewBlock]): ProcessingState = {
    if(peerToSend != this.peer) {
      if(!state.toBroadcastBlocksToEachPeer.contains(peerToSend)) context watch peerToSend
      peerToSend ! GetMaxBlockNumber(self)

      val prevToBroadcastBlocksToEachPeer = state.toBroadcastBlocksToEachPeer.getOrElse(peerToSend, Nil)
      val newToBroadcastBlocksToEachPeer = state.toBroadcastBlocksToEachPeer + (peerToSend -> (prevToBroadcastBlocksToEachPeer ++ blocksToSend))
      state.withToBroadcastBlocksToEachPeer(newToBroadcastBlocksToEachPeer)
    }
    else state
  }

  /**
    * This function handles the MaxBlockNumber message, by sending to the peer that sent it the new blocks that it doesn't know.
    */
  def handleMaxBlockNumber(state: ProcessingState): Receive = {
    case MaxBlockNumber(maxBlockNumber) =>
      val blocksToSend = state.toBroadcastBlocksToEachPeer.getOrElse(sender(), Nil)
      blocksToSend.foreach { newBlockMsg =>
        if(maxBlockNumber < newBlockMsg.block.header.number){
          log.info("Sending NewBlockMessage {} to {}", newBlockMsg, sender().path.name)
          sender() ! PeerActor.SendMessage(newBlockMsg)
        }
      }
      context become processMessages(state.withToBroadcastBlocksToEachPeer(state.toBroadcastBlocksToEachPeer - sender()))
  }

  def handlePeerTerminated: Receive = {
    case PeerTerminated(terminatedPeer, state) =>
      context unwatch terminatedPeer
      context become processMessages(state.withToBroadcastBlocksToEachPeer(state.toBroadcastBlocksToEachPeer - terminatedPeer))
  }
}

object MaxBlockNumberRequestHandler {
  case class RequestMaxBlockNumber(peers: Seq[ActorRef], blocksToSend: Seq[NewBlock])
  case class PeerTerminated(peer: ActorRef, state: ProcessingState)
}
