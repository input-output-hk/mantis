package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.network.{Peer, PeerActor}
import io.iohk.ethereum.network.PeersInfoHolderActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock

trait BlockBroadcast { self: SyncController =>

  //FIXME: Decide block propagation algorithm (for now we send block to every peer) [EC-87]
  def broadcastNewBlocks(newBlocks: Seq[NewBlock], handshakedPeers: Map[Peer, PeerInfo]): Unit = {
    val blocksForEachPeer: Seq[(Peer, NewBlock)] = for {
      (peer, peerInfo) <- handshakedPeers.toSeq
      newBlock <- newBlocks
      if peerInfo.maxBlockNumber < newBlock.block.header.number
    } yield (peer, newBlock)
    blocksForEachPeer.foreach{ case (peer, newBlock) => peer.ref ! PeerActor.SendMessage(newBlock) }
  }

}
