package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import akka.event.LoggingReceive
import io.iohk.ethereum.blockchain.sync.BlockBroadcast
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.Config.SyncConfig

class NewRegularSync(
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {
  import NewRegularSync._

  val fetcher: ActorRef =
    context.actorOf(BlockFetcher.props(etcPeerManager, peerEventBus, syncConfig, scheduler), "block-fetcher")
  val importer: ActorRef =
    context.actorOf(BlockImporter.props(fetcher, ledger, blockchain, syncConfig), "block-importer")
  val broadcaster: ActorRef = context.actorOf(
    BlockBroadcasterActor
      .props(new BlockBroadcast(etcPeerManager, syncConfig), peerEventBus, etcPeerManager, syncConfig, scheduler))

  override def receive: Receive = LoggingReceive {
    case Start => importer ! BlockImporter.Start
  }
}
object NewRegularSync {
  def props(
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props = Props(new NewRegularSync(etcPeerManager, peerEventBus, ledger, blockchain, syncConfig, scheduler))

  sealed trait NewRegularSyncMsg
  case object Start extends NewRegularSyncMsg
}
