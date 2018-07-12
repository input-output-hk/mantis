package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.transactions.PendingTransactionsManager.TransactionsToFilter
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions

class SignedTransactionsFilterActor(pendingTransactionsManager: ActorRef) extends Actor{

  override def receive: Receive = {
    case TransactionsToFilter(signedTransactions, peerId) =>
      val correctTransactions = signedTransactions.filter(tx => SignedTransaction.getSender(tx).isDefined)
      pendingTransactionsManager ! ProperSignedTransactions(correctTransactions.toSet, peerId)
  }
}

object SignedTransactionsFilterActor {
  def props(pendingTransactionsManager: ActorRef): Props =
    Props(new SignedTransactionsFilterActor(pendingTransactionsManager))

  case class ProperSignedTransactions(signedTransactions: Set[SignedTransaction], peerId: PeerId)
}