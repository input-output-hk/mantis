package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.PeerMessageBusActor.{MessageClassifier, MessageFromPeer, PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions

object PendingTransactionsManager {
  def props(peerManager: ActorRef, peerMessageBus: ActorRef): Props =
    Props(new PendingTransactionsManager(peerManager, peerMessageBus))

  case class BroadcastTransaction(signedTransaction: SignedTransaction)

  case object GetPendingTransactions
  case class PendingTransactions(signedTransactions: Seq[SignedTransaction])

  case class RemoveTransactions(signedTransactions: Seq[SignedTransaction])
}

class PendingTransactionsManager(peerManager: ActorRef, peerMessageBus: ActorRef) extends Actor {

  import PendingTransactionsManager._

  var pendingTransactions: Seq[SignedTransaction] = Nil

  peerMessageBus ! Subscribe(MessageClassifier(Set(SignedTransactions.code), PeerSelector.AllPeers))

  override def receive: Receive = {
    case BroadcastTransaction(signedTransaction) =>
      peerManager ! PeerManagerActor.BroadcastMessage(SignedTransactions(Seq(signedTransaction)))
      pendingTransactions :+= signedTransaction

    case GetPendingTransactions =>
      sender() ! PendingTransactions(pendingTransactions)

    case RemoveTransactions(signedTransactions) =>
      pendingTransactions = pendingTransactions.filterNot(signedTransactions.contains)

    case MessageFromPeer(signedTransactions: SignedTransactions, peerId) =>
      signedTransactions.txs.foreach { signedTransaction =>
        if (!pendingTransactions.contains(signedTransaction))
          pendingTransactions :+= signedTransaction
      }
  }

}
