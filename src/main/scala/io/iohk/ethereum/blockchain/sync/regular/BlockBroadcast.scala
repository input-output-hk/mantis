package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.util.Random

class BlockBroadcast(val etcPeerManager: ActorRef, syncConfig: SyncConfig) {

  /**
    * Broadcasts various NewBlock's messages to handshaked peers, considering that a block should not be sent to a peer
    * that is thought to know it.
    * The hash of the block is sent to all of those peers while the block itself is only sent to
    * the square root of the total number of those peers, with the subset being obtained randomly.
    *
    * @param newBlock, block to broadcast
    * @param handshakedPeers, to which the blocks will be broadcasted to
    */
  def broadcastBlock(newBlock: NewBlock, handshakedPeers: Map[Peer, PeerInfo]): Unit = {
    val peersWithoutBlock = handshakedPeers.collect {
      case (peer, peerInfo) if shouldSendNewBlock(newBlock, peerInfo) => peer
    }.toSet

    broadcastNewBlock(newBlock, peersWithoutBlock)

    if (syncConfig.broadcastNewBlockHashes) {
      // NOTE: the usefulness of this message is debatable, especially in private networks
      broadcastNewBlockHash(newBlock, peersWithoutBlock)
    }
  }

  private def shouldSendNewBlock(newBlock: NewBlock, peerInfo: PeerInfo): Boolean =
    newBlock.block.header.number > peerInfo.maxBlockNumber ||
      newBlock.chainWeight > peerInfo.chainWeight

  private def broadcastNewBlock(newBlock: NewBlock, peers: Set[Peer]): Unit =
    obtainRandomPeerSubset(peers).foreach { peer =>
      etcPeerManager ! EtcPeerManagerActor.SendMessage(newBlock, peer.id)
    }

  private def broadcastNewBlockHash(newBlock: NewBlock, peers: Set[Peer]): Unit = peers.foreach { peer =>
    val newBlockHeader = newBlock.block.header
    val newBlockHashMsg = PV62.NewBlockHashes(Seq(BlockHash(newBlockHeader.hash, newBlockHeader.number)))
    etcPeerManager ! EtcPeerManagerActor.SendMessage(newBlockHashMsg, peer.id)
  }

  /**
    * Obtains a random subset of peers. The returned set will verify:
    *   subsetPeers.size == sqrt(peers.size)
    *
    * @param peers
    * @return a random subset of peers
    */
  private[sync] def obtainRandomPeerSubset(peers: Set[Peer]): Set[Peer] = {
    val numberOfPeersToSend = Math.sqrt(peers.size).toInt
    Random.shuffle(peers).take(numberOfPeersToSend)
  }
}
