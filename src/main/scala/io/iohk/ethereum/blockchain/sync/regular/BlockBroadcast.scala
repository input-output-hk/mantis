package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.domain.{Block, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import io.iohk.ethereum.network.p2p.messages.{PV60, PV62, PV64, ProtocolVersions}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}

import scala.util.Random

class BlockBroadcast(val etcPeerManager: ActorRef) {

  /**
    * Broadcasts various NewBlock's messages to handshaked peers, considering that a block should not be sent to a peer
    * that is thought to know it.
    * The hash of the block is sent to all of those peers while the block itself is only sent to
    * the square root of the total number of those peers, with the subset being obtained randomly.
    *
    * @param blockToBroadcast, block to broadcast
    * @param handshakedPeers, to which the blocks will be broadcasted to
    */
  def broadcastBlock(blockToBroadcast: BlockToBroadcast, handshakedPeers: Map[Peer, PeerInfo]): Unit = {
    val peersWithoutBlock = handshakedPeers.filter { case (_, peerInfo) =>
      shouldSendNewBlock(blockToBroadcast, peerInfo)
    }

    broadcastNewBlock(blockToBroadcast, peersWithoutBlock)

    broadcastNewBlockHash(blockToBroadcast, peersWithoutBlock.keySet)
  }

  private def shouldSendNewBlock(newBlock: BlockToBroadcast, peerInfo: PeerInfo): Boolean =
    newBlock.block.header.number > peerInfo.maxBlockNumber ||
      newBlock.chainWeight > peerInfo.chainWeight

  private def broadcastNewBlock(blockToBroadcast: BlockToBroadcast, peers: Map[Peer, PeerInfo]): Unit =
    obtainRandomPeerSubset(peers.keySet).foreach { peer =>
      val message: MessageSerializable =
        if (peers(peer).remoteStatus.protocolVersion == ProtocolVersions.PV64) blockToBroadcast.as64
        else blockToBroadcast.as63
      etcPeerManager ! EtcPeerManagerActor.SendMessage(message, peer.id)
    }

  private def broadcastNewBlockHash(blockToBroadcast: BlockToBroadcast, peers: Set[Peer]): Unit = peers.foreach {
    peer =>
      val newBlockHeader = blockToBroadcast.block.header
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
    Random.shuffle(peers.toSeq).take(numberOfPeersToSend).toSet
  }
}

object BlockBroadcast {

  /**
    * BlockToBroadcast was created to decouple block information from protocol new block messages
    * (they are different versions of NewBlock msg)
    */
  case class BlockToBroadcast(block: Block, chainWeight: ChainWeight) {
    def as63: PV60.NewBlock = PV60.NewBlock(block, chainWeight.totalDifficulty)
    def as64: PV64.NewBlock = PV64.NewBlock(block, chainWeight)
  }
}
