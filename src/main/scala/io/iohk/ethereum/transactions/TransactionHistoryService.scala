package io.iohk.ethereum.transactions

import akka.actor.ActorRef
import cats.implicits._
import akka.util.Timeout
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, Blockchain, SignedTransaction}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.transactions.TransactionHistoryService.{ExtendedTransactionData, TxChecker}
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
  def getAccountTransactions(
      account: Address,
      fromBlocks: NumericRange[BigInt]
  ): Task[List[ExtendedTransactionData]] = {
    val txnsFromBlocks = Observable
      .from(fromBlocks.reverse)
      .mapParallelOrdered(10)(blockNr => Task { blockchain.getBlockByNumber(blockNr) })(OverflowStrategy.Unbounded)
      .collect { case Some(block) => block }
      .concatMapIterable { block =>
        val checker = TxChecker.forSigned(block)
        block.body.transactionList.toList
          .collect(Function.unlift(checker.checkTx(_, account)))
          .reverse
      }
      .toListL

    val txnsFromMempool = getTransactionsFromPool map { pendingTransactions =>
      pendingTransactions
        .collect(Function.unlift(TxChecker.forPending.checkTx(_, account)))
    }

    Task.parMap2(txnsFromBlocks, txnsFromMempool)(_ ++ _)
  }

  private val getTransactionsFromPool: Task[List[PendingTransaction]] = {
    implicit val timeout: Timeout = getTransactionFromPoolTimeout
    pendingTransactionsManager
      .askFor[PendingTransactionsManager.PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .map(_.pendingTransactions.toList)
      .onErrorRecoverWith { case ex: Throwable =>
        log.error("Failed to get pending transactions, passing empty transactions list", ex)
        Task.now(List.empty)
      }
  }
}
object TransactionHistoryService {
  case class ExtendedTransactionData(
      stx: SignedTransaction,
      isOutgoing: Boolean,
      //block header and transaction index
      minedTransactionData: Option[(BlockHeader, Int)]
  ) {
    val isPending: Boolean = minedTransactionData.isEmpty
  }

  trait TxChecker[T] {
    def checkTx(tx: T, address: Address): Option[ExtendedTransactionData]
  }
  object TxChecker {
    val forPending: TxChecker[PendingTransaction] = new TxChecker[PendingTransaction] {
      def isSender(tx: PendingTransaction, maybeSender: Address) = tx.stx.senderAddress == maybeSender
      def isReceiver(tx: PendingTransaction, maybeReceiver: Address) =
        tx.stx.tx.tx.receivingAddress.contains(maybeReceiver)
      def asSigned(tx: PendingTransaction) = tx.stx.tx

      def checkTx(tx: PendingTransaction, address: Address): Option[ExtendedTransactionData] = {
        if (isSender(tx, address)) {
          Some(ExtendedTransactionData(asSigned(tx), isOutgoing = true, None))
        } else if (isReceiver(tx, address)) {
          Some(ExtendedTransactionData(asSigned(tx), isOutgoing = false, None))
        } else {
          None
        }
      }
    }

    def forSigned(block: Block): TxChecker[SignedTransaction] = new TxChecker[SignedTransaction] {
      def isSender(tx: SignedTransaction, maybeSender: Address) = tx.safeSenderIsEqualTo(maybeSender)
      def isReceiver(tx: SignedTransaction, maybeReceiver: Address) = tx.tx.receivingAddress.contains(maybeReceiver)
      def asSigned(tx: SignedTransaction) = tx

      def getMinedTxData(tx: SignedTransaction): Option[(BlockHeader, Int)] = {
        val maybeIndex = block.body.transactionList.zipWithIndex.collectFirst {
          case (someTx, index) if someTx.hash == tx.hash => index
        }

        (Some(block.header), maybeIndex).tupled
      }

      def checkTx(tx: SignedTransaction, address: Address): Option[ExtendedTransactionData] = {
        if (isSender(tx, address)) {
          Some(ExtendedTransactionData(asSigned(tx), isOutgoing = true, getMinedTxData(tx)))
        } else if (isReceiver(tx, address)) {
          Some(ExtendedTransactionData(asSigned(tx), isOutgoing = false, getMinedTxData(tx)))
        } else {
          None
        }
      }
    }
  }
}
