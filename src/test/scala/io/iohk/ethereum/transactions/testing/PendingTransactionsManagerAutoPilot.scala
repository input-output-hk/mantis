package io.iohk.ethereum.transactions.testing
import akka.actor.ActorRef
import akka.testkit.TestActor.AutoPilot
import akka.util.ByteString
import io.iohk.ethereum.domain.{SignedTransaction, SignedTransactionWithSender}
import io.iohk.ethereum.transactions.PendingTransactionsManager._
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions

case class PendingTransactionsManagerAutoPilot(pendingTransactions: Set[PendingTransaction] = Set.empty)
    extends AutoPilot {
  def run(sender: ActorRef, msg: Any): AutoPilot =
    msg match {
      case AddUncheckedTransactions(transactions) =>
        val validTxs = SignedTransactionWithSender.getSignedTransactions(transactions)
        this.addTransactions(validTxs.toSet)

      case AddTransactions(signedTransactions) =>
        this.addTransactions(signedTransactions)

      case AddOrOverrideTransaction(newStx) =>
        // Only validated transactions are added this way, it is safe to call get
        val newStxSender = SignedTransaction.getSender(newStx).get
        val obsoleteTxs = pendingTransactions
          .filter(ptx => ptx.stx.senderAddress == newStxSender && ptx.stx.tx.tx.nonce == newStx.tx.nonce)
          .map(_.stx.tx.hash)

        removeTransactions(obsoleteTxs).addTransactions(Set(SignedTransactionWithSender(newStx, newStxSender)))

      case GetPendingTransactions =>
        sender ! PendingTransactionsResponse(pendingTransactions.toSeq)
        this

      case RemoveTransactions(signedTransactions) =>
        this.removeTransactions(signedTransactions.map(_.hash).toSet)

      case ProperSignedTransactions(transactions, peerId) =>
        this.addTransactions(transactions)

      case ClearPendingTransactions =>
        copy(pendingTransactions = Set.empty)
    }

  def addTransactions(signedTransactions: Set[SignedTransactionWithSender]): PendingTransactionsManagerAutoPilot = {
    val timestamp = System.currentTimeMillis()
    val stxs = pendingTransactions.map(_.stx)
    val transactionsToAdd = signedTransactions.diff(stxs).map(tx => PendingTransaction(tx, timestamp))

    copy(pendingTransactions ++ transactionsToAdd)
  }

  def removeTransactions(hashes: Set[ByteString]): PendingTransactionsManagerAutoPilot =
    copy(pendingTransactions.filterNot(ptx => hashes.contains(ptx.stx.tx.hash)))
}
