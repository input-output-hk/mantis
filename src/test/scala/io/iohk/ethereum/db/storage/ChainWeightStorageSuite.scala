package io.iohk.ethereum.db.storage

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource

class ChainWeightStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("ChainWeightStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val weightList = Gen.listOf(chainWeightGen).sample.get
      val blockHashesWeightsPairs = weightList.zip(blockHashes)

      val storage = new ChainWeightStorage(EphemDataSource())
      val batchUpdates = blockHashesWeightsPairs.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (weight, blockHash)) =>
          updates.and(storage.put(blockHash, weight))
      }
      batchUpdates.commit()

      blockHashesWeightsPairs.foreach { case (weight, blockHash) => assert(storage.get(blockHash).contains(weight)) }
    }
  }

  test("ChainWeightStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val weightList = Gen.listOf(chainWeightGen).sample.get
      val blockHashesWeightsPairs = weightList.zip(blockHashes)

      //Chain weight of blocks is inserted
      val storage = new ChainWeightStorage(EphemDataSource())
      val storageInsertions = blockHashesWeightsPairs.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (td, blockHash)) =>
          updates.and(storage.put(blockHash, td))
      }
      storageInsertions.commit()

      //Chain weight of blocks is deleted
      val (toDelete, toLeave) = blockHashesWeightsPairs.splitAt(Gen.choose(0, blockHashesWeightsPairs.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (_, blockHash)) =>
        updates.and(storage.remove(blockHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (weight, blockHeader) =>
        assert(storage.get(blockHeader).contains(weight))
      }
      toDelete.foreach { case (_, bh) => assert(storage.get(bh).isEmpty) }
    }
  }
}
