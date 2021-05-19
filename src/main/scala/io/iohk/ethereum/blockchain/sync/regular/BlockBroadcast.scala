package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.domain.{Block, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import io.iohk.ethereum.network.p2p.messages.{CommonMessages, PV62, PV164, ProtocolVersions}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}

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
  def broadcastBlock(blockToBroadcast: BlockToBroadcast, handshakedPeers: Map[PeerId, PeerWithInfo]): Unit = {
    val peersWithoutBlock = handshakedPeers.filter { case (_, PeerWithInfo(_, peerInfo)) =>
      shouldSendNewBlock(blockToBroadcast, peerInfo)
    }

    broadcastNewBlock(blockToBroadcast, peersWithoutBlock)

    broadcastNewBlockHash(blockToBroadcast, peersWithoutBlock.values.map(_.peer).toSet)
  }

  private def shouldSendNewBlock(newBlock: BlockToBroadcast, peerInfo: PeerInfo): Boolean =
    newBlock.block.header.number > peerInfo.maxBlockNumber ||
      newBlock.chainWeight > peerInfo.chainWeight

  private def broadcastNewBlock(blockToBroadcast: BlockToBroadcast, peers: Map[PeerId, PeerWithInfo]): Unit =
    obtainRandomPeerSubset(peers.values.map(_.peer).toSet).foreach { peer =>
      val message: MessageSerializable =
        if (peers(peer.id).peerInfo.remoteStatus.protocolVersion == ProtocolVersions.PV164) blockToBroadcast.as64
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
    def as63: CommonMessages.NewBlock = CommonMessages.NewBlock(block, chainWeight.totalDifficulty)
    def as64: PV164.NewBlock = PV164.NewBlock(block, chainWeight)
  }
}
