package io.iohk.ethereum.transactions

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.dispatch.BoundedMessageQueueSemantics
import akka.dispatch.RequiresMessageQueue

import io.iohk.ethereum.domain.SignedTransactionWithSender
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.BlockchainConfig

class SignedTransactionsFilterActor(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef)
    extends Actor
    with RequiresMessageQueue[BoundedMessageQueueSemantics] {

  implicit val blockchainConfig: BlockchainConfig = Config.blockchains.blockchainConfig

  peerEventBus ! Subscribe(MessageClassifier(Set(Codes.SignedTransactionsCode), PeerSelector.AllPeers))

  override def receive: Receive = { case MessageFromPeer(SignedTransactions(newTransactions), peerId) =>
    val correctTransactions = SignedTransactionWithSender.getSignedTransactions(newTransactions)
    pendingTransactionsManager ! ProperSignedTransactions(correctTransactions.toSet, peerId)
  }
}

object SignedTransactionsFilterActor {
  def props(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef): Props =
    Props(new SignedTransactionsFilterActor(pendingTransactionsManager, peerEventBus))

  case class ProperSignedTransactions(signedTransactions: Set[SignedTransactionWithSender], peerId: PeerId)
}
