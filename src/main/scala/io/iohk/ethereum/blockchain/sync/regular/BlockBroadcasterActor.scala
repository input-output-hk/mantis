package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.{Blacklist, PeerListSupportNg}
import io.iohk.ethereum.utils.Config.SyncConfig

class BlockBroadcasterActor(
    broadcast: BlockBroadcast,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val blacklist: Blacklist,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupportNg {
  import BlockBroadcasterActor._

  override def receive: Receive = handlePeerListMessages orElse handleBroadcastMessages

  private def handleBroadcastMessages: Receive = {
    case BroadcastBlock(newBlock) => broadcast.broadcastBlock(newBlock, handshakedPeers)
    case BroadcastBlocks(blocks) => blocks.foreach(broadcast.broadcastBlock(_, handshakedPeers))
  }
}
object BlockBroadcasterActor {
  sealed trait BroadcasterMsg
  case class BroadcastBlock(block: BlockToBroadcast) extends BroadcasterMsg
  case class BroadcastBlocks(blocks: List[BlockToBroadcast]) extends BroadcasterMsg

  def props(
      broadcast: BlockBroadcast,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new BlockBroadcasterActor(
        broadcast = broadcast,
        peerEventBus = peerEventBus,
        etcPeerManager = etcPeerManager,
        blacklist = blacklist,
        syncConfig = syncConfig,
        scheduler = scheduler
      )
    )
}
