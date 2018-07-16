package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions

class SignedTransactionsFilterActor(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef) extends Actor{

  peerEventBus ! Subscribe(MessageClassifier(Set(SignedTransactions.code), PeerSelector.AllPeers))

  override def receive: Receive = {
    case MessageFromPeer(SignedTransactions(newTransactions), peerId) =>
      val correctTransactions = newTransactions.filter(tx => SignedTransaction.getSender(tx).isDefined)
      pendingTransactionsManager ! ProperSignedTransactions(correctTransactions.toSet, peerId)
  }
}

object SignedTransactionsFilterActor {
  def props(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef): Props =
    Props(new SignedTransactionsFilterActor(pendingTransactionsManager, peerEventBus))

  case class ProperSignedTransactions(signedTransactions: Set[SignedTransaction], peerId: PeerId)
}
