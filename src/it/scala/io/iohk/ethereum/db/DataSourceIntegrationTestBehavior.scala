package io.iohk.ethereum.db

import java.io.File
import java.nio.file.Files

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.{DataSource, DataSourceUpdate}
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.iohk.ethereum.utils.ByteStringUtils._

trait DataSourceIntegrationTestBehavior extends ScalaCheckPropertyChecks with ObjectGenerators {

  this: AnyFlatSpec =>

  val KeySizeWithoutPrefix: Int = 32
  val KeySize: Int = KeySizeWithoutPrefix + 1
  //Hash size + prefix
  val KeyNumberLimit: Int = 40
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)

  val MaxIncreaseInLength = 10

  def withDir(testCode: String => Any): Unit = {
    val path = Files.createTempDirectory("testdb").getFileName.toString
    try {
      testCode(path)
    } finally {
      val dir = new File(path)
      assert(!dir.exists() || dir.delete(), "File deletion failed")
    }
  }

  def prepareUpdate(
      namespace: Namespace = OtherNamespace,
      toRemove: Seq[Key] = Nil,
      toUpsert: Seq[(Key, Value)] = Nil
  ): Seq[DataSourceUpdate] =
    Seq(DataSourceUpdate(namespace, toRemove, toUpsert))

  def updateInSeparateCalls(
      dataSource: DataSource,
      toUpsert: Seq[(ByteString, ByteString)]
  ): Unit = {
    toUpsert.foreach { keyValuePair =>
      dataSource.update(prepareUpdate(toUpsert = Seq(keyValuePair)))
    }
  }

  // scalastyle:off
  def dataSource(createDataSource: => String => DataSource): Unit = {
    it should "be able to insert keys in separate updates" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          updateInSeparateCalls(
            dataSource = db,
            toUpsert = keyList.zip(keyList)
          )
          keyList.foreach { key =>
            assert(db.get(OtherNamespace, key).contains(key))
          }

          db.destroy()
        }
      }
    }

    it should "be able to insert keys in a single update" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))

          keyList.foreach { key =>
            assert(db.get(OtherNamespace, key).contains(key))
          }

          db.destroy()
        }
      }
    }

    it should "be able to update keys in separate updates" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))

          val keyListWithExtraByte = keyList.map(key => concatByteStrings(1.toByte, key))
          updateInSeparateCalls(db, keyList.zip(keyListWithExtraByte))

          keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
            assert(db.get(OtherNamespace, key).contains(value))
          }

          db.destroy()
        }
      }
    }

    it should "be able to update keys in a single update" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))

          val keyListWithExtraByte = keyList.map(1.toByte +: _)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyListWithExtraByte)))

          keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
            assert(db.get(OtherNamespace, key).contains(value))
          }

          db.destroy()
        }
      }
    }

    it should "be cleared" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))
          db.clear()

          keyList.foreach { key =>
            assert(db.get(OtherNamespace, key).isEmpty)
          }

          db.destroy()
        }
      }
    }

    it should "be able to be closed and then continuing using it" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))
          db.close()

          val dbAfterClose = createDataSource(path)
          keyList.foreach { key =>
            assert(dbAfterClose.get(OtherNamespace, key).contains(key))
          }

          dbAfterClose.destroy()
        }
      }
    }

    it should "be destroyed" in {
      withDir { path =>
        forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)
          db.update(prepareUpdate(toUpsert = keyList.zip(keyList)))
          db.destroy()

          assert(!new File("/tmp/iodbDestroy").exists())

          val dbAfterDestroy = createDataSource(path)
          keyList.foreach { key =>
            assert(dbAfterDestroy.get(OtherNamespace, key).isEmpty)
          }

          dbAfterDestroy.destroy()
        }
      }
    }

    it should "be able to handle inserts to multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)

          val valList1 = keyList.map(1.toByte +: _)
          db.update(prepareUpdate(namespace = OtherNamespace, toUpsert = keyList.zip(valList1)))

          val valList2 = keyList.map(2.toByte +: _)
          db.update(prepareUpdate(namespace = OtherNamespace2, toUpsert = keyList.zip(valList2)))

          keyList.zip(valList1).foreach { case (key, value) =>
            assert(db.get(OtherNamespace, key).contains(value))
          }
          keyList.zip(valList2).foreach { case (key, value) =>
            assert(db.get(OtherNamespace2, key).contains(value))
          }

          db.destroy()
        }
      }
    }

    it should "be able to handle removals from multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path)

          val valList1 = keyList.map(1.toByte +: _)
          db.update(prepareUpdate(namespace = OtherNamespace, toUpsert = keyList.zip(valList1)))

          val valList2 = keyList.map(2.toByte +: _)
          db.update(prepareUpdate(namespace = OtherNamespace2, toUpsert = keyList.zip(valList2)))

          //Removal of keys from the OtherNamespace namespace
          db.update(prepareUpdate(namespace = OtherNamespace, toRemove = keyList))

          keyList.foreach { key =>
            assert(db.get(OtherNamespace, key).isEmpty)
          }
          keyList.zip(valList2).foreach { case (key, value) =>
            assert(db.get(OtherNamespace2, key).contains(value))
          }

          //Removal of keys from the OtherNamespace2 namespace
          db.update(prepareUpdate(namespace = OtherNamespace2, toRemove = keyList))

          keyList.foreach { key =>
            assert(db.get(OtherNamespace, key).isEmpty)
          }
          keyList.foreach { key =>
            assert(db.get(OtherNamespace2, key).isEmpty)
          }
          db.destroy()
        }
      }
    }
  }
  // scalastyle:on

}
