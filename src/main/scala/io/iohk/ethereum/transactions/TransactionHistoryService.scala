package io.iohk.ethereum.transactions

import akka.actor.ActorRef
import akka.util.Timeout
import cats.implicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.transactions.TransactionHistoryService.{
  ExtendedTransactionData,
  MinedTxChecker,
  PendingTxChecker
}
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import monix.reactive.{Observable, OverflowStrategy}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration
import scala.language.higherKinds

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
      .concatMap { block =>
        val getBlockReceipts = Task {
          blockchain.getReceiptsByHash(block.hash).map(_.toVector).getOrElse(Vector.empty)
        }.memoizeOnSuccess

        Observable
          .from(block.body.transactionList.reverse)
          .collect(Function.unlift(MinedTxChecker.checkTx(_, account)))
          .mapEval { case (tx, mkExtendedData) =>
            getBlockReceipts.map(MinedTxChecker.getMinedTxData(tx, block, _).map(mkExtendedData(_)))
          }
          .collect { case Some(data) =>
            data
          }
      }
      .toListL

    val txnsFromMempool = getTransactionsFromPool map { pendingTransactions =>
      pendingTransactions
        .collect(Function.unlift(PendingTxChecker.checkTx(_, account)))
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
  case class MinedTransactionData(
      header: BlockHeader,
      transactionIndex: Int,
      gasUsed: BigInt
  ) {
    lazy val timestamp: Long = header.unixTimestamp
  }
  case class ExtendedTransactionData(
      stx: SignedTransaction,
      isOutgoing: Boolean,
      minedTransactionData: Option[MinedTransactionData]
  ) {
    val isPending: Boolean = minedTransactionData.isEmpty
  }

  object PendingTxChecker {
    def isSender(tx: PendingTransaction, maybeSender: Address): Boolean = tx.stx.senderAddress == maybeSender
    def isReceiver(tx: PendingTransaction, maybeReceiver: Address): Boolean =
      tx.stx.tx.tx.receivingAddress.contains(maybeReceiver)
    def asSigned(tx: PendingTransaction): SignedTransaction = tx.stx.tx

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

  object MinedTxChecker {
    def isSender(tx: SignedTransaction, maybeSender: Address): Boolean = tx.safeSenderIsEqualTo(maybeSender)
    def isReceiver(tx: SignedTransaction, maybeReceiver: Address): Boolean =
      tx.tx.receivingAddress.contains(maybeReceiver)

    def checkTx(
        tx: SignedTransaction,
        address: Address
    ): Option[(SignedTransaction, MinedTransactionData => ExtendedTransactionData)] = {
      if (isSender(tx, address)) {
        Some((tx, data => ExtendedTransactionData(tx, isOutgoing = true, Some(data))))
      } else if (isReceiver(tx, address)) {
        Some((tx, data => ExtendedTransactionData(tx, isOutgoing = false, Some(data))))
      } else {
        None
      }
    }

    def getMinedTxData(
        tx: SignedTransaction,
        block: Block,
        blockReceipts: Vector[Receipt]
    ): Option[MinedTransactionData] = {
      val maybeIndex = block.body.transactionList.zipWithIndex.collectFirst {
        case (someTx, index) if someTx.hash == tx.hash => index
      }

      val maybeGasUsed = for {
        index <- maybeIndex
        txReceipt <- blockReceipts.lift(index)
      } yield {
        val previousCumulativeGas: BigInt =
          (if (index > 0) blockReceipts.lift(index - 1) else None).map(_.cumulativeGasUsed).getOrElse(0)

        txReceipt.cumulativeGasUsed - previousCumulativeGas
      }

      (Some(block.header), maybeIndex, maybeGasUsed).mapN(MinedTransactionData)
    }
  }
}
