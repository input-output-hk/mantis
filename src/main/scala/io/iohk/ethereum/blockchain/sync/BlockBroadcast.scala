package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorRef
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock

trait BlockBroadcast {

  val etcPeerManager: ActorRef

  /**
    * Broadcasts various NewBlock's messages to handshaked peers, considering that a block should not be sent to a peer
    * that is thought to know it. In the current implementation we send every block to every peer (that doesn't know
    * this block)
    *
    * @param newBlocks, blocks to broadcast
    * @param handshakedPeers
    */
  //FIXME: Decide block propagation algorithm (for now we send block to every peer) [EC-87]
  def broadcastNewBlocks(newBlocks: Seq[NewBlock], handshakedPeers: Map[Peer, PeerInfo]): Unit = {
    val blocksForEachPeer: Seq[(Peer, NewBlock)] = for {
      (peer, peerInfo) <- handshakedPeers.toSeq
      newBlock <- newBlocks
      if shouldSendNewBlock(newBlock, peerInfo)
    } yield (peer, newBlock)
    blocksForEachPeer.foreach{ case (peer, newBlock) => etcPeerManager ! EtcPeerManagerActor.SendMessage(newBlock, peer.id) }
  }

  private def shouldSendNewBlock(newBlock: NewBlock, peerInfo: PeerInfo): Boolean =
    newBlock.block.header.number > peerInfo.maxBlockNumber || newBlock.totalDifficulty > peerInfo.totalDifficulty

}
