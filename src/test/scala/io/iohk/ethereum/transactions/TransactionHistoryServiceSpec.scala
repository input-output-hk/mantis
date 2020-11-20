package io.iohk.ethereum.transactions

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Address, Block, SignedTransaction, Transaction}
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import io.iohk.ethereum.transactions.testing.PendingTransactionsManagerAutoPilot
import monix.eval.Task

import java.security.SecureRandom

class TransactionHistoryServiceSpec
    extends TestKit(ActorSystem("TransactionHistoryServiceSpec-system"))
    with FreeSpecBase
    with SpecFixtures
    with WithActorSystemShutDown {
  class Fixture extends EphemBlockchainTestSetup {
    val pendingTransactionManager = TestProbe()
    pendingTransactionManager.setAutoPilot(PendingTransactionsManagerAutoPilot())
    val transactionHistoryService =
      new TransactionHistoryService(blockchain, pendingTransactionManager.ref, Timeouts.normalTimeout)
  }

  def createFixture() = new Fixture

  "returns account recent transactions in newest -> oldest order" in testCaseM { fixture =>
    import fixture._

    val address = Address("0xee4439beb5c71513b080bbf9393441697a29f478")

    val keyPair = crypto.generateKeyPair(new SecureRandom)

    val tx1 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 1, ByteString()), keyPair, None).tx
    val tx2 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 2, ByteString()), keyPair, None).tx
    val tx3 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 3, ByteString()), keyPair, None).tx

    val blockWithTx1 =
      Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body.copy(transactionList = Seq(tx1)))

    val blockWithTxs2and3 = Block(
      Fixtures.Blocks.Block3125369.header.copy(number = 3125370),
      Fixtures.Blocks.Block3125369.body.copy(transactionList = Seq(tx2, tx3))
    )

    val expectedTxs = Seq(
      ExtendedTransactionData(
        tx3,
        isOutgoing = false,
        Some(blockWithTxs2and3.header -> 1)
      ),
      ExtendedTransactionData(
        tx2,
        isOutgoing = false,
        Some(blockWithTxs2and3.header -> 0)
      ),
      ExtendedTransactionData(tx1, isOutgoing = false, Some(blockWithTx1.header -> 0))
    )

    for {
      _ <- Task {
        blockchain
          .storeBlock(blockWithTx1)
          .and(blockchain.storeBlock(blockWithTxs2and3))
          .commit()
      }
      response <- transactionHistoryService.getAccountTransactions(address, BigInt(3125360) to BigInt(3125370))
    } yield assert(response === expectedTxs)
  }

  "does not return account recent transactions from older blocks and return pending txs" in testCaseM { fixture =>
    import fixture._

    val blockWithTx = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    val keyPair = crypto.generateKeyPair(new SecureRandom)

    val tx = Transaction(0, 123, 456, None, 99, ByteString())
    val signedTx = SignedTransaction.sign(tx, keyPair, None)

    val expectedSent =
      Seq(ExtendedTransactionData(signedTx.tx, isOutgoing = true, None))

    for {
      _ <- Task { blockchain.storeBlock(blockWithTx).commit() }
      _ <- Task { pendingTransactionManager.ref ! PendingTransactionsManager.AddTransactions(signedTx) }
      response <- transactionHistoryService.getAccountTransactions(
        signedTx.senderAddress,
        BigInt(3125371) to BigInt(3125381)
      )
    } yield assert(response === expectedSent)
  }
}
