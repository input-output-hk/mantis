package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

class SignedTransactionsFilterActor(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef) extends Actor{

  peerEventBus ! Subscribe(MessageClassifier(Set(SignedTransactions.code), PeerSelector.AllPeers))

  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool())

  override def receive: Receive = {
    case MessageFromPeer(SignedTransactions(newTransactions), peerId) =>
      val correctTransactions = Future {
        val txs = newTransactions.filter(tx => SignedTransaction.getSender(tx).isDefined).toSet
        ProperSignedTransactions(txs, peerId)
      }
      correctTransactions pipeTo pendingTransactionsManager
  }
}

object SignedTransactionsFilterActor {
  def props(pendingTransactionsManager: ActorRef, peerEventBus: ActorRef): Props =
    Props(new SignedTransactionsFilterActor(pendingTransactionsManager, peerEventBus))

  case class ProperSignedTransactions(signedTransactions: Set[SignedTransaction], peerId: PeerId)
}
