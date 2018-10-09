package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, AllForOneStrategy, Props, Scheduler, SupervisorStrategy}
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
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {
  import NewRegularSync._

  val fetcher: ActorRef =
    context.actorOf(BlockFetcher.props(etcPeerManager, peerEventBus, syncConfig, scheduler), "block-fetcher")
  val broadcaster: ActorRef = context.actorOf(
    BlockBroadcasterActor
      .props(new BlockBroadcast(etcPeerManager, syncConfig), peerEventBus, etcPeerManager, syncConfig, scheduler))
  val importer: ActorRef =
    context.actorOf(
      BlockImporter.props(fetcher, ledger, blockchain, syncConfig, ommersPool, broadcaster, pendingTransactionsManager),
      "block-importer")

  override def receive: Receive = {
    case Start => importer ! BlockImporter.Start
  }

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy()(SupervisorStrategy.defaultDecider)
}
object NewRegularSync {
  def props(
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      pendingTransactionsManager: ActorRef,
      scheduler: Scheduler
  ): Props =
    Props(
      new NewRegularSync(
        etcPeerManager,
        peerEventBus,
        ledger,
        blockchain,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        scheduler))

  sealed trait NewRegularSyncMsg
  case object Start extends NewRegularSyncMsg
}
