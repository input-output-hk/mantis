package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.domain.{Block, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHash
import io.iohk.ethereum.network.p2p.messages.{CommonMessages, PV62, PV64, Versions}
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
  def broadcastBlock(newBlock: BlockToBroadcast, handshakedPeers: Map[Peer, PeerInfo]): Unit = {
    val peersWithoutBlock = handshakedPeers.filter { case (_, peerInfo) =>
      shouldSendNewBlock(newBlock, peerInfo)
    }

    broadcastNewBlock(newBlock, peersWithoutBlock)

    if (syncConfig.broadcastNewBlockHashes) {
      // NOTE: the usefulness of this message is debatable, especially in private networks
      broadcastNewBlockHash(newBlock, peersWithoutBlock.keySet)
    }
  }

  private def shouldSendNewBlock(newBlock: BlockToBroadcast, peerInfo: PeerInfo): Boolean =
    newBlock.block.header.number > peerInfo.maxBlockNumber ||
      newBlock.chainWeight > peerInfo.chainWeight

  private def broadcastNewBlock(newBlock: BlockToBroadcast, peers: Map[Peer, PeerInfo]): Unit =
    obtainRandomPeerSubset(peers.keySet).foreach { peer =>
      val message: MessageSerializable =
        if (peers(peer).remoteStatus.protocolVersion == Versions.PV64) newBlock.as64 else newBlock.as63
      etcPeerManager ! EtcPeerManagerActor.SendMessage(message, peer.id)
    }

  private def broadcastNewBlockHash(newBlock: BlockToBroadcast, peers: Set[Peer]): Unit = peers.foreach { peer =>
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

object BlockBroadcast {

  /**
    * BlockToBroadcast was created to decouple block information from protocol new block messages
    * (they are different versions of NewBlock msg)
    */
  case class BlockToBroadcast(block: Block, chainWeight: ChainWeight) {
    def as63: CommonMessages.NewBlock = CommonMessages.NewBlock(block, chainWeight.totalDifficulty)
    def as64: PV64.NewBlock = PV64.NewBlock(block, chainWeight)
  }
  object BlockToBroadcast {
    def apply(block: CommonMessages.NewBlock): BlockToBroadcast =
      BlockToBroadcast(block.block, ChainWeight.totalDifficultyOnly(block.totalDifficulty))

    def apply(block: PV64.NewBlock): BlockToBroadcast =
      BlockToBroadcast(block.block, block.chainWeight)
  }
}
