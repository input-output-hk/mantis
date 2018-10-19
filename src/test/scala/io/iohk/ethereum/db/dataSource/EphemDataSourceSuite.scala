package io.iohk.ethereum.db.dataSource

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class EphemDataSourceSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {

  val KeySize: Int = 32
  val KeyNumberLimit: Int = 40
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('e'.toByte)
  def putMultiple(dataSource: DataSource, toInsert: Seq[(ByteString, ByteString)]): DataSource = {
    toInsert.foldLeft(dataSource){ case (recDB, (key, value)) =>
      val actualKey = key.toArray[Byte]
      recDB.update(OtherNamespace, Seq(), Seq((actualKey, value)))
    }
  }

  def removeMultiple(dataSource: DataSource, toDelete: Seq[ByteString]): DataSource = {
    toDelete.foldLeft(dataSource){ case (recDB, key) =>
      recDB.update(OtherNamespace, Seq(key.toArray[Byte]), Seq())
    }
  }

  test("EphemDataSource insert"){
    forAll(seqByteStringOfNItemsGen(KeySize)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.filter(_.length == KeySize)
      val db = putMultiple(dataSource = EphemDataSource(), toInsert = keyList.zip(keyList))
      keyList.foreach { key =>
        val actualKey = key.toArray[Byte]
        val obtained = db.get(OtherNamespace, actualKey)
        assert(obtained.isDefined)
        assert(obtained.get == key)
      }
    }
  }

  test("EphemDataSource delete"){
    forAll(seqByteStringOfNItemsGen(KeySize)) { keyList: Seq[ByteString] =>
      val (keysToDelete, keyValueLeft) = keyList.splitAt(Gen.choose(0, keyList.size).sample.get)

      val dbAfterInsert = putMultiple(dataSource = EphemDataSource(), toInsert = keyList.zip(keyList))
      val db = removeMultiple(dataSource = dbAfterInsert, toDelete = keysToDelete)
      keyValueLeft.foreach { key =>
        val actualKey = key.toArray[Byte]
        val obtained = db.get(OtherNamespace, actualKey)
        assert(obtained.isDefined)
        assert(obtained.get == key)
      }
      keysToDelete.foreach { key => assert(db.get(OtherNamespace, key.toArray[Byte]).isEmpty) }
    }
  }

  test("EphemDataSource clear") {
    forAll(seqByteStringOfNItemsGen(KeySize)) { keyList: Seq[ByteString] =>
      val keys = keyList.map(k => k.toArray[Byte])
      val db = EphemDataSource()
        .update(OtherNamespace, toRemove = Seq(), toUpsert = keys.zip(keyList))
        .clear

      keys.foreach { key => assert(db.get(OtherNamespace, key).isEmpty) }
    }
  }
}
