package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class CodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  val LimitCodeSize = 100

  test("CodeStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ unfilteredCodeHashes =>
      val codeHashes = unfilteredCodeHashes.distinct
      val codes = Gen.listOfN(codeHashes.length, randomSizeByteArrayGen(0, LimitCodeSize)).sample.get.map(ByteString(_))
      val initialCodeStorage = new EvmCodeStorage(EphemDataSource())
      val codeStorage = codeHashes.zip(codes).foldLeft(initialCodeStorage){
        case (recCodeStorage, (codeHash, code)) =>
          recCodeStorage.put(codeHash, code)
      }

      codeHashes.zip(codes).foreach{ case (codeHash, code) =>
        assert(codeStorage.get(codeHash).contains(code))
      }
    }
  }

  test("CodeStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ unfilteredCodeHashes =>
      val codeHashes = unfilteredCodeHashes.distinct
      val codes = Gen.listOfN(codeHashes.length, randomSizeByteArrayGen(0, LimitCodeSize)).sample.get.map(ByteString(_))

      //EVM codes are inserted
      val initialCodeStorage = new EvmCodeStorage(EphemDataSource())
      val codeStorage = codeHashes.zip(codes).foldLeft(initialCodeStorage){
        case (recCodeStorage, (codeHash, code)) =>
          recCodeStorage.put(codeHash, code)
      }

      //EVM codes are deleted
      val (toDelete, toLeave) = Random.shuffle(codeHashes.zip(codes))
        .splitAt(Gen.choose(0, codeHashes.size).sample.get)
      val codeStorageAfterDelete = toDelete.foldLeft(codeStorage){
        case (recCodeStorage, (codeHash, _)) =>
          recCodeStorage.remove(codeHash)
      }

      toLeave.foreach{ case (codeHash, code) => assert(codeStorageAfterDelete.get(codeHash).contains(code)) }
      toDelete.foreach{ case (codeHash, _) => assert(codeStorageAfterDelete.get(codeHash).isEmpty) }
    }
  }
}
