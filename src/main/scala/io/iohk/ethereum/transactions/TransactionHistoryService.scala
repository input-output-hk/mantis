package io.iohk.ethereum.transactions

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.domain.{Address, BlockHeader, Blockchain, SignedTransaction}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.jsonrpc.TransactionResponse
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.transactions.TransactionHistoryService.TxChecker
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import monix.reactive.{Observable, OverflowStrategy}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration

class TransactionHistoryService(
    blockchain: Blockchain,
    pendingTransactionsManager: ActorRef,
    getTransactionFromPoolTimeout: FiniteDuration
) extends Logger {
  def getAccountTransactions(account: Address, fromBlocks: NumericRange[BigInt]): Task[Seq[TransactionResponse]] = {
    val txnsFromBlocks = Observable
      .from(fromBlocks.reverse)
      .mapParallelOrdered(10)(blockNr => Task { blockchain.getBlockByNumber(blockNr) })(OverflowStrategy.Unbounded)
      .collect { case Some(block) => block }
      .concatMapIterable { block =>
        val checker = TxChecker.forSigned(block.header)
        block.body.transactionList.toVector
          .collect(Function.unlift(checker.checkTx(_, account)))
          .reverse
      }
      .toListL

    val txnsFromMempool = getTransactionsFromPool map { pendingTransactions =>
      pendingTransactions
        .collect(Function.unlift(TxChecker.forPending.checkTx(_, account)))
    }

    Task.parMap2(txnsFromBlocks, txnsFromMempool)((pending, fromBlocks) => (pending ++ fromBlocks))
  }

  private val getTransactionsFromPool: Task[Vector[PendingTransaction]] = {
    implicit val timeout: Timeout = getTransactionFromPoolTimeout
    pendingTransactionsManager
      .askFor[PendingTransactionsManager.PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .map(_.pendingTransactions.toVector)
      .onErrorRecoverWith { case ex: Throwable =>
        log.error("Failed to get pending transactions, passing empty transactions list", ex)
        Task.now(Vector.empty)
      }
  }
}
object TransactionHistoryService {
  trait TxChecker[T] {
    def isTxPending: Boolean
    def maybeBlockHeader: Option[BlockHeader]
    def isSender(tx: T, address: Address): Boolean
    def isReceiver(tx: T, address: Address): Boolean
    def asSigned(tx: T): SignedTransaction

    def checkTx(tx: T, address: Address): Option[TransactionResponse] = {
      if (isSender(tx, address)) {
        Some(
          TransactionResponse(
            asSigned(tx),
            maybeBlockHeader,
            pending = Some(isTxPending),
            isOutgoing = Some(true)
          )
        )
      } else if (isReceiver(tx, address)) {
        Some(
          TransactionResponse(
            asSigned(tx),
            maybeBlockHeader,
            pending = Some(isTxPending),
            isOutgoing = Some(false)
          )
        )
      } else {
        None
      }
    }
  }
  object TxChecker {
    val forPending: TxChecker[PendingTransaction] = new TxChecker[PendingTransaction] {
      val isTxPending = true
      val maybeBlockHeader = None
      def isSender(tx: PendingTransaction, maybeSender: Address) = tx.stx.senderAddress == maybeSender
      def isReceiver(tx: PendingTransaction, maybeReceiver: Address) =
        tx.stx.tx.tx.receivingAddress.contains(maybeReceiver)
      def asSigned(tx: PendingTransaction) = tx.stx.tx
    }

    def forSigned(blockHeader: BlockHeader): TxChecker[SignedTransaction] = new TxChecker[SignedTransaction] {
      val isTxPending = false
      val maybeBlockHeader = Some(blockHeader)
      def isSender(tx: SignedTransaction, maybeSender: Address) = tx.safeSenderIsEqualTo(maybeSender)
      def isReceiver(tx: SignedTransaction, maybeReceiver: Address) = tx.tx.receivingAddress.contains(maybeReceiver)
      def asSigned(tx: SignedTransaction) = tx
    }
  }
}
