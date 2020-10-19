package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class TotalDifficultyStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("TotalDifficultyStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val tdList = Gen.listOf(bigIntGen).sample.get
      val blockHashesTdPair = tdList.zip(blockHashes)

      val storage = new TotalDifficultyStorage(EphemDataSource())
      val batchUpdates = blockHashesTdPair.foldLeft(storage.emptyBatchUpdate) { case (updates, (td, blockHash)) =>
        updates.and(storage.put(blockHash, td))
      }
      batchUpdates.commit()

      blockHashesTdPair.foreach { case (td, blockHash) => assert(storage.get(blockHash).contains(td)) }
    }
  }

  test("TotalDifficultyStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val tdList = Gen.listOf(bigIntGen).sample.get
      val blockHashesTdPair = tdList.zip(blockHashes)

      //Total difficulty of blocks is inserted
      val storage = new TotalDifficultyStorage(EphemDataSource())
      val storageInsertions = blockHashesTdPair.foldLeft(storage.emptyBatchUpdate) { case (updates, (td, blockHash)) =>
        updates.and(storage.put(blockHash, td))
      }
      storageInsertions.commit()

      //Total difficulty of blocks is deleted
      val (toDelete, toLeave) = blockHashesTdPair.splitAt(Gen.choose(0, blockHashesTdPair.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (_, blockHash)) =>
        updates.and(storage.remove(blockHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (td, blockHeader) =>
        assert(storage.get(blockHeader).contains(td))
      }
      toDelete.foreach { case (_, bh) => assert(storage.get(bh).isEmpty) }
    }
  }
}
