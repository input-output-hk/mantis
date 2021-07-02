package io.iohk.ethereum.db.storage

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.domain.Receipt

class ReceiptStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {

  test("ReceiptStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      val storage = new ReceiptStorage(EphemDataSource())
      val batchUpdates = blockHashesReceiptsPair.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (receiptList, blockHash)) =>
          updates.and(storage.put(blockHash, receiptList))
      }
      batchUpdates.commit()

      blockHashesReceiptsPair.foreach { case (rs, bh) =>
        val obtainedReceipts: Option[Seq[Receipt]] = storage.get(bh)
        assert(obtainedReceipts.contains(rs))
      }
    }
  }

  test("ReceiptStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      //Receipts are inserted
      val storage = new ReceiptStorage(EphemDataSource())
      val storageInsertions = blockHashesReceiptsPair.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (receiptList, blockHash)) =>
          updates.and(storage.put(blockHash, receiptList))
      }
      storageInsertions.commit()

      //Receipts are deleted
      val (toDelete, toLeave) = blockHashesReceiptsPair.splitAt(Gen.choose(0, blockHashesReceiptsPair.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (_, blockHash)) =>
        updates.and(storage.remove(blockHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (rs, bh) =>
        val obtainedReceipts = storage.get(bh)
        assert(obtainedReceipts.contains(rs))
      }
      toDelete.foreach { case (_, bh) => assert(storage.get(bh).isEmpty) }
    }
  }
}
