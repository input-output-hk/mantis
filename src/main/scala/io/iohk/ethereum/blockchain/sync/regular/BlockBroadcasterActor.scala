package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Scheduler
import io.iohk.ethereum.blockchain.sync.BlacklistSupport
import io.iohk.ethereum.blockchain.sync.PeerListSupport
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.utils.Config.SyncConfig

class BlockBroadcasterActor(
    broadcast: BlockBroadcast,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {
  import BlockBroadcasterActor._

  override def receive: Receive = handlePeerListMessages.orElse(handleBlacklistMessages).orElse(handleBroadcastMessages)

  private def handleBroadcastMessages: Receive = {
    case BroadcastBlock(newBlock) => broadcast.broadcastBlock(newBlock, handshakedPeers)
    case BroadcastBlocks(blocks)  => blocks.foreach(broadcast.broadcastBlock(_, handshakedPeers))
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
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new BlockBroadcasterActor(
        broadcast = broadcast,
        peerEventBus = peerEventBus,
        etcPeerManager = etcPeerManager,
        syncConfig = syncConfig,
        scheduler = scheduler
      )
    )
}
