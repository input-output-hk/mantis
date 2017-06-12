package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class TransactionMappingStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("TotalDifficultyStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ txByteArrayHashes =>
      val txHashes = txByteArrayHashes.distinct
      val blockHashesList = Gen.listOfN(txByteArrayHashes.length, byteStringOfLengthNGen(32)).sample.get
      val txIndexList = Gen.listOfN(txByteArrayHashes.length, intGen).sample.get
      val txLocationList = blockHashesList.zip(txIndexList).map{ case (blockHash, txIndex) =>
        TransactionLocation(blockHash, txIndex) }

      val initialTxMappingStorage = new TransactionMappingStorage(EphemDataSource())
      val txMappingStorage = txHashes.zip(txLocationList).foldLeft(initialTxMappingStorage){
        case (recTxMappingStorage, (txHash, txLocation)) =>
          recTxMappingStorage.put(txHash, txLocation)
      }

      txHashes.zip(txLocationList).foreach{case (txHash, txLocation) => assert(txMappingStorage.get(txHash).contains(txLocation)) }
    }
  }

  test("TotalDifficultyStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ txByteArrayHashes =>
      val txHashes = txByteArrayHashes.distinct
      val blockHashesList = Gen.listOfN(txByteArrayHashes.length, byteStringOfLengthNGen(32)).sample.get
      val txIndexList = Gen.listOfN(txByteArrayHashes.length, intGen).sample.get
      val txLocationList = blockHashesList.zip(txIndexList).map{ case (blockHash, txIndex) =>
        TransactionLocation(blockHash, txIndex) }
      val txHashAndLocationPair = txHashes.zip(txLocationList)

      //Total difficulty of blocks is inserted
      val initialTxMappingStorage = new TransactionMappingStorage(EphemDataSource())
      val txMappingStorage = txHashAndLocationPair.foldLeft(initialTxMappingStorage){
        case (recTxMappingStorage, (txHash, txLocation)) =>
          recTxMappingStorage.put(txHash, txLocation)
      }

      //Total difficulty of blocks is deleted
      val (toDelete, toLeave) = txHashAndLocationPair.splitAt(Gen.choose(0, txHashAndLocationPair.size).sample.get)
      val txMappingStorageAfterDelete = toDelete.foldLeft(txMappingStorage){
        case (recTxMappingStorage, (txHash, _)) =>
          recTxMappingStorage.remove(txHash)
      }

      toLeave.foreach{case (txHash, txLocation) => assert(txMappingStorageAfterDelete.get(txHash).contains(txLocation)) }
      toDelete.foreach{ case (txHash, _) => assert(txMappingStorageAfterDelete.get(txHash).isEmpty) }
    }
  }
}
