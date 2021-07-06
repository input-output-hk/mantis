package io.iohk.ethereum.transactions

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import monix.eval.Task

import com.softwaremill.diffx.scalatest.DiffMatcher
import mouse.all._
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.domain._
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import io.iohk.ethereum.transactions.TransactionHistoryService.MinedTransactionData
import io.iohk.ethereum.transactions.testing.PendingTransactionsManagerAutoPilot
import io.iohk.ethereum.{blockchain => _, _}

class LegacyTransactionHistoryServiceSpec
    extends TestKit(ActorSystem("TransactionHistoryServiceSpec-system"))
    with FreeSpecBase
    with SpecFixtures
    with WithActorSystemShutDown
    with Matchers
    with DiffMatcher {
  class Fixture extends EphemBlockchainTestSetup {
    val pendingTransactionManager: TestProbe = TestProbe()
    pendingTransactionManager.setAutoPilot(PendingTransactionsManagerAutoPilot())
    val transactionHistoryService =
      new TransactionHistoryService(blockchain, blockchainReader, pendingTransactionManager.ref, Timeouts.normalTimeout)
  }

  def createFixture() = new Fixture

  "returns account recent transactions in newest -> oldest order" in testCaseM { fixture: Fixture =>
    import fixture._

    val address = Address("ee4439beb5c71513b080bbf9393441697a29f478")

    val keyPair = generateKeyPair(secureRandom)

    val tx1 = SignedTransaction.sign(LegacyTransaction(0, 123, 456, Some(address), 1, ByteString()), keyPair, None).tx
    val tx2 = SignedTransaction.sign(LegacyTransaction(0, 123, 456, Some(address), 2, ByteString()), keyPair, None).tx
    val tx3 = SignedTransaction.sign(LegacyTransaction(0, 123, 456, Some(address), 3, ByteString()), keyPair, None).tx

    val blockWithTx1 =
      Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body.copy(transactionList = Seq(tx1)))
    val blockTx1Receipts = Seq(Receipt(HashOutcome(ByteString("foo")), 42, ByteString.empty, Nil))

    val blockWithTxs2and3 = Block(
      Fixtures.Blocks.Block3125369.header.copy(number = 3125370),
      Fixtures.Blocks.Block3125369.body.copy(transactionList = Seq(tx2, tx3))
    )
    val blockTx2And3Receipts = Seq(
      Receipt(HashOutcome(ByteString("bar")), 43, ByteString.empty, Nil),
      Receipt(HashOutcome(ByteString("baz")), 43 + 44, ByteString.empty, Nil)
    )

    val expectedTxs = Seq(
      ExtendedTransactionData(
        tx3,
        isOutgoing = false,
        Some(MinedTransactionData(blockWithTxs2and3.header, 1, 44, isCheckpointed = false))
      ),
      ExtendedTransactionData(
        tx2,
        isOutgoing = false,
        Some(MinedTransactionData(blockWithTxs2and3.header, 0, 43, isCheckpointed = false))
      ),
      ExtendedTransactionData(
        tx1,
        isOutgoing = false,
        Some(MinedTransactionData(blockWithTx1.header, 0, 42, isCheckpointed = false))
      )
    )

    for {
      _ <- Task {
        blockchainWriter
          .storeBlock(blockWithTx1)
          .and(blockchainWriter.storeReceipts(blockWithTx1.hash, blockTx1Receipts))
          .and(blockchainWriter.storeBlock(blockWithTxs2and3))
          .and(blockchainWriter.storeReceipts(blockWithTxs2and3.hash, blockTx2And3Receipts))
          .commit()
      }
      response <- transactionHistoryService.getAccountTransactions(address, BigInt(3125360) to BigInt(3125370))
    } yield assert(response === expectedTxs)
  }

  "does not return account recent transactions from older blocks and return pending txs" in testCaseM {
    fixture: Fixture =>
      import fixture._

      val blockWithTx = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

      val keyPair = generateKeyPair(secureRandom)

      val tx = LegacyTransaction(0, 123, 456, None, 99, ByteString())
      val signedTx = SignedTransaction.sign(tx, keyPair, None)

      val expectedSent =
        Seq(ExtendedTransactionData(signedTx.tx, isOutgoing = true, None))

      for {
        _ <- Task(blockchainWriter.storeBlock(blockWithTx).commit())
        _ <- Task(pendingTransactionManager.ref ! PendingTransactionsManager.AddTransactions(signedTx))
        response <- transactionHistoryService.getAccountTransactions(
          signedTx.senderAddress,
          BigInt(3125371) to BigInt(3125381)
        )
      } yield assert(response === expectedSent)
  }

  "marks transactions as checkpointed if there's checkpoint block following block containing transaction" in testCaseM {
    fixture: Fixture =>
      import fixture._

      val keyPair = generateKeyPair(secureRandom)
      val senderAddress = Address(keyPair)
      val checkpointKey = generateKeyPair(secureRandom)

      val txToBeCheckpointed = LegacyTransaction(0, 123, 456, None, 99, ByteString())
      val signedTxToBeCheckpointed = SignedTransaction.sign(txToBeCheckpointed, keyPair, None)

      val txNotToBeCheckpointed =
        LegacyTransaction(1, 123, 456, Address("ee4439beb5c71513b080bbf9393441697a29f478"), 99, ByteString())
      val signedTxNotToBeCheckpointed = SignedTransaction.sign(txNotToBeCheckpointed, keyPair, None)

      val block1 = BlockHelpers
        .generateBlock(BlockHelpers.genesis) |> (BlockHelpers.withTransactions(_, List(signedTxToBeCheckpointed.tx)))
      val block2 = BlockHelpers.generateBlock(block1) |> (
        BlockHelpers.updateHeader(
          _,
          header => {
            val checkpoint =
              Checkpoint(List(ECDSASignature.sign(crypto.kec256(ByteString("foo")).toArray, checkpointKey, None)))
            header.copy(extraFields = HefPostEcip1097(Some(checkpoint)))
          }
        )
      )
      val block3 =
        BlockHelpers.generateBlock(block2) |> (BlockHelpers.withTransactions(_, List(signedTxNotToBeCheckpointed.tx)))

      val expectedCheckpointedTxData = ExtendedTransactionData(
        signedTxToBeCheckpointed.tx,
        isOutgoing = true,
        Some(MinedTransactionData(block1.header, 0, 21000, isCheckpointed = true))
      )
      val expectedNonCheckpointedTxData = ExtendedTransactionData(
        signedTxNotToBeCheckpointed.tx,
        isOutgoing = true,
        Some(MinedTransactionData(block3.header, 0, 21000, isCheckpointed = false))
      )

      def makeReceipts(block: Block): Seq[Receipt] =
        block.body.transactionList.map(tx => Receipt(HashOutcome(block.hash), BigInt(21000), ByteString("foo"), Nil))

      for {
        _ <- Task {
          blockchainWriter.save(block1, makeReceipts(block1), ChainWeight(0, block1.header.difficulty), true)
        }
        _ <- Task(blockchainWriter.save(block2, Nil, ChainWeight(2, block1.header.difficulty), true))
        _ <- Task {
          blockchainWriter.save(block3, makeReceipts(block3), ChainWeight(2, block1.header.difficulty * 2), true)
        }
        lastCheckpoint <- Task(blockchain.getLatestCheckpointBlockNumber())
        response <- transactionHistoryService.getAccountTransactions(
          senderAddress,
          BigInt.apply(0) to BigInt(10)
        )
      } yield {
        assert(!block3.hasCheckpoint)
        assert(lastCheckpoint === block2.number)
        assert(block2.hasCheckpoint)
        response should matchTo(List(expectedNonCheckpointedTxData, expectedCheckpointedTxData))
      }
  }
}
