package io.iohk.ethereum.db.dataSource

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class EphemDataSourceSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {

  val KeySize: Int = 32
  val KeyNumberLimit: Int = 40
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('e'.toByte)

  def putMultiple(dataSource: DataSource, toInsert: Seq[(ByteString, ByteString)]): Unit =
    toInsert.foreach { keyValuePair =>
      dataSource.update(Seq(DataSourceUpdate(OtherNamespace, Seq(), Seq(keyValuePair))))
    }

  def removeMultiple(dataSource: DataSource, toDelete: Seq[ByteString]): Unit =
    toDelete.foreach { key =>
      dataSource.update(Seq(DataSourceUpdate(OtherNamespace, Seq(key), Seq())))
    }

  test("EphemDataSource insert") {
    forAll(seqByteStringOfNItemsGen(KeySize)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.filter(_.length == KeySize)
      val db = EphemDataSource()
      putMultiple(dataSource = db, toInsert = keyList.zip(keyList))
      keyList.foreach { key =>
        val obtained = db.get(OtherNamespace, key)
        assert(obtained.isDefined)
        assert(obtained.get.sameElements(key))
      }
    }
  }

  test("EphemDataSource delete") {
    forAll(seqByteStringOfNItemsGen(KeySize)) { keyList: Seq[ByteString] =>
      val (keysToDelete, keyValueLeft) = keyList.splitAt(Gen.choose(0, keyList.size).sample.get)

      val db = EphemDataSource()
      putMultiple(dataSource = db, toInsert = keyList.zip(keyList))
      removeMultiple(dataSource = db, toDelete = keysToDelete)

      keyValueLeft.foreach { key =>
        val obtained = db.get(OtherNamespace, key)
        assert(obtained.isDefined)
        assert(obtained.get.sameElements(key))
      }
      keysToDelete.foreach { key =>
        assert(db.get(OtherNamespace, key).isEmpty)
      }
    }
  }

  test("EphemDataSource clear") {
    forAll(seqByteStringOfNItemsGen(KeySize)) { keyList: Seq[ByteString] =>
      val db = EphemDataSource()

      putMultiple(db, keyList.zip(keyList))
      db.clear()

      keyList.foreach { key =>
        assert(db.get(OtherNamespace, key).isEmpty)
      }
    }
  }
}
