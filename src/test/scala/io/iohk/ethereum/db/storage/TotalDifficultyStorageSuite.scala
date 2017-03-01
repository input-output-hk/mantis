package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class TotalDifficultyStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("TotalDifficultyStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val tdList = Gen.listOf(bigIntGen).sample.get
      val blockHashesTdPair = tdList.zip(blockHashes)

      val initialTotalDifficultyStorage = new TotalDifficultyStorage(EphemDataSource())
      val totalDifficultyStorage = blockHashesTdPair.foldLeft(initialTotalDifficultyStorage){
        case (recTotalDifficultyStorage, (td, blockHash)) =>
          recTotalDifficultyStorage.put(blockHash, td)
      }

      blockHashesTdPair.foreach{case (td, blockHash) => assert(totalDifficultyStorage.get(blockHash).contains(td)) }
    }
  }

  test("TotalDifficultyStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val tdList = Gen.listOf(bigIntGen).sample.get
      val blockHashesTdPair = tdList.zip(blockHashes)

      //Total difficulty of blocks is inserted
      val initialTotalDifficultyStorage = new TotalDifficultyStorage(EphemDataSource())
      val totalDifficultyStorage = blockHashesTdPair.foldLeft(initialTotalDifficultyStorage){
        case (recTotalDifficultyStorage, (td, blockHash)) =>
          recTotalDifficultyStorage.put(blockHash, td)
      }

      //Total difficulty of blocks is deleted
      val (toDelete, toLeave) = blockHashesTdPair.splitAt(Gen.choose(0, blockHashesTdPair.size).sample.get)
      val totalDifficultyStorageAfterDelete = toDelete.foldLeft(totalDifficultyStorage){
        case (recTotalDifficultyStorage, (_, blockHash)) =>
          recTotalDifficultyStorage.remove(blockHash)
      }

      toLeave.foreach{case (td, blockHeader) => assert(totalDifficultyStorageAfterDelete.get(blockHeader).contains(td)) }
      toDelete.foreach{ case (_, bh) => assert(totalDifficultyStorageAfterDelete.get(bh).isEmpty) }
    }
  }
}
