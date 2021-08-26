package io.iohk.ethereum.db.storage

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource

class BlockMetadataStorageSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("BlockMetadataStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val metadataList = Gen.listOf(blockMetadataGen).sample.get
      val hashesMetadataPairs = blockHashes.zip(metadataList)

      val storage = new BlockMetadataStorage(EphemDataSource())
      val batchUpdates = hashesMetadataPairs.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (blockHash, metadata)) => updates.and(storage.put(blockHash, metadata))
      }
      batchUpdates.commit()

      hashesMetadataPairs.foreach { case (blockHash, metadata) =>
        assert(storage.get(blockHash).contains(metadata))
      }
    }
  }

  test("BlockMetadataStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val metadataList = Gen.listOf(blockMetadataGen).sample.get
      val hashesMetadataPairs = blockHashes.zip(metadataList)

      // insert data
      val storage = new BlockMetadataStorage(EphemDataSource())
      val batchUpdates = hashesMetadataPairs.foldLeft(storage.emptyBatchUpdate) {
        case (updates, (blockHash, metadata)) => updates.and(storage.put(blockHash, metadata))
      }
      batchUpdates.commit()

      //delete data
      val (toDelete, toLeave) = hashesMetadataPairs.splitAt(Gen.choose(0, hashesMetadataPairs.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (blockHash, _)) =>
        updates.and(storage.remove(blockHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (hash, metadata) =>
        assert(storage.get(hash).contains(metadata))
      }
      toDelete.foreach { case (hash, _) => assert(storage.get(hash).isEmpty) }
    }
  }

  val blockMetadataGen: Gen[BlockMetadata] = for {
    isExecuted <- Arbitrary.arbitrary[Boolean]
  } yield BlockMetadata(isExecuted)
}
