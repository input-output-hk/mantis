package io.iohk.ethereum.db.dataSource

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
  def putMultiple(dataSource: DataSource, toInsert: Seq[(Array[Byte], Array[Byte])]): DataSource = {
    toInsert.foldLeft(dataSource){ case (recDB, (key, value)) =>
      recDB.update(OtherNamespace, Seq(), Seq((key, value)))
    }
  }

  def removeMultiple(dataSource: DataSource, toDelete: Seq[Array[Byte]]): DataSource = {
    toDelete.foldLeft(dataSource){ case (recDB, key) =>
      recDB.update(OtherNamespace, Seq(key), Seq())
    }
  }

  test("EphemDataSource insert"){
    forAll(seqByteArrayOfNItemsGen(KeySize)) { unFilteredKeyList =>
      val keyList = unFilteredKeyList.filter(_.length == KeySize)
      val db = putMultiple(dataSource = EphemDataSource(), toInsert = keyList.zip(keyList))
      keyList.foreach { key =>
        val obtained = db.get(OtherNamespace, key)
        assert(obtained.isDefined)
        assert(obtained.get sameElements key)
      }
    }
  }

  test("EphemDataSource delete"){
    forAll(seqByteArrayOfNItemsGen(KeySize)) { keyList =>
      val (keysToDelete, keyValueLeft) = keyList.splitAt(Gen.choose(0, keyList.size).sample.get)

      val dbAfterInsert = putMultiple(dataSource = EphemDataSource(), toInsert = keyList.zip(keyList))
      val db = removeMultiple(dataSource = dbAfterInsert, toDelete = keysToDelete)
      keyValueLeft.foreach { key =>
        val obtained = db.get(OtherNamespace, key)
        assert(obtained.isDefined)
        assert(obtained.get sameElements key)
      }
      keysToDelete.foreach { key => assert(db.get(OtherNamespace, key).isEmpty) }
    }
  }

  test("EphemDataSource clear") {
    forAll(seqByteArrayOfNItemsGen(KeySize)) { keyList =>
      val db = EphemDataSource()
        .update(OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
        .clear

      keyList.foreach { key => assert(db.get(OtherNamespace, key).isEmpty) }
    }
  }
}
