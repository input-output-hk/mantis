package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class CodeStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  val LimitCodeSize = 100

  test("CodeStorage insert") {

    val storage = new EvmCodeStorage(EphemDataSource())
    val hashes = Seq(ByteString("asdf"), ByteString("asdf"))
    val codes = Seq(ByteString("qwerty"), ByteString("poiu"))
    val batchUpdates = hashes.zip(codes).foldLeft(storage.emptyBatchUpdate) { case (updates, (codeHash, code)) =>
      updates.and(storage.put(codeHash, code))
    }
    batchUpdates.commit()
    hashes.zip(codes).foreach { case (codeHash, code) =>
      assert(storage.get(codeHash).contains(code))
    }
  }

  test("CodeStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))) { unfilteredCodeHashes =>
      val codeHashes = unfilteredCodeHashes.distinct
      val codes = Gen.listOfN(codeHashes.length, randomSizeByteArrayGen(0, LimitCodeSize)).sample.get.map(ByteString(_))

      //EVM codes are inserted
      val storage = new EvmCodeStorage(EphemDataSource())
      val storageInsertions =
        codeHashes.zip(codes).foldLeft(storage.emptyBatchUpdate) { case (updates, (codeHash, code)) =>
          updates.and(storage.put(codeHash, code))
        }
      storageInsertions.commit()

      //EVM codes are deleted
      val (toDelete, toLeave) = codeHashes
        .zip(codes)
        .splitAt(Gen.choose(0, codeHashes.size).sample.get)
      val storageDeletions = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, (codeHash, _)) =>
        updates.and(storage.remove(codeHash))
      }
      storageDeletions.commit()

      toLeave.foreach { case (codeHash, code) => assert(storage.get(codeHash).contains(code)) }
      toDelete.foreach { case (codeHash, _) => assert(storage.get(codeHash).isEmpty) }
    }
  }
}
