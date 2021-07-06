package io.iohk.ethereum.db.storage

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation

class TransactionMappingStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("TransactionMappingStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { txByteArrayHashes =>
      val txHashes = txByteArrayHashes.distinct
      val blockHashesList = Gen.listOfN(txByteArrayHashes.length, byteStringOfLengthNGen(32)).sample.get
      val txIndexList = Gen.listOfN(txByteArrayHashes.length, intGen).sample.get
      val txLocationList = blockHashesList.zip(txIndexList).map { case (blockHash, txIndex) =>
        TransactionLocation(blockHash, txIndex)
      }

      val storage = new TransactionMappingStorage(EphemDataSource())
      val batchUpdates =
        txHashes.zip(txLocationList).foldLeft(storage.emptyBatchUpdate) { case (updates, (txHash, txLocation)) =>
          updates.and(storage.put(txHash, txLocation))
        }
      batchUpdates.commit()

      txHashes.zip(txLocationList).foreach { case (txHash, txLocation) =>
        assert(storage.get(txHash).contains(txLocation))
      }
    }
  }

  test("TransactionMappingStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { txByteArrayHashes =>
      val txHashes = txByteArrayHashes.distinct
      val blockHashesList = Gen.listOfN(txByteArrayHashes.length, byteStringOfLengthNGen(32)).sample.get
      val txIndexList = Gen.listOfN(txByteArrayHashes.length, intGen).sample.get
      val txLocationList = blockHashesList.zip(txIndexList).map { case (blockHash, txIndex) =>
        TransactionLocation(blockHash, txIndex)
      }
      val txHashAndLocationPair = txHashes.zip(txLocationList)

      //Mapping of tx to blocks is inserted
      val storage = new TransactionMappingStorage(EphemDataSource())
      val storageInsertions = txHashAndLocationPair.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (txHash, txLocation)) =>
          updates.and(storage.put(txHash, txLocation))
      }
      storageInsertions.commit()

      //Mapping of tx to blocks is deleted
      val (toDelete, toLeave) = txHashAndLocationPair.splitAt(Gen.choose(0, txHashAndLocationPair.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (txHash, _)) =>
        updates.and(storage.remove(txHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (txHash, txLocation) =>
        assert(storage.get(txHash).contains(txLocation))
      }
      toDelete.foreach { case (txHash, _) => assert(storage.get(txHash).isEmpty) }
    }
  }
}
