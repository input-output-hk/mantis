package io.iohk.ethereum.db

import java.io.File
import java.nio.file.Files

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.DataSource
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

trait DataSourceIntegrationTestBehavior
  extends PropertyChecks
    with ObjectGenerators {

  this: FlatSpec =>

  val KeySizeWithoutPrefix: Int = 32
  val KeySize: Int = KeySizeWithoutPrefix + 1
  //Hash size + prefix
  val KeyNumberLimit: Int = 40
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('e'.toByte)

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

  def updateInSeparateCalls(dataSource: DataSource, toUpsert: Seq[(ByteString, ByteString)]): DataSource = {
    toUpsert.foldLeft(dataSource) { case (recDB, keyValuePair) =>
      recDB.update(OtherNamespace, Seq(), Seq(keyValuePair))
    }
  }

  // scalastyle:off
  def dataSource(createDataSource: => String => DataSource): Unit = {
    it should "be able to insert keys in separate updates" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = updateInSeparateCalls(
            dataSource = createDataSource(path),
            toUpsert = keyList.zip(keyList)
          )
          keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }

          db.destroy()
        }
      }
    }

    it should "be able to insert keys in a single update" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(OtherNamespace, Seq(), keyList.zip(keyList))

          keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }

          db.destroy()
        }
      }
    }

    it should "be able to update keys in separate updates" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(OtherNamespace, Seq(), keyList.zip(keyList))

          val keyListWithExtraByte = keyList.map(1.toByte +: _)
          val dbAfterUpdate = updateInSeparateCalls(db, keyList.zip(keyListWithExtraByte))

          keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
            assert(dbAfterUpdate.get(OtherNamespace, key).contains(value))
          }

          dbAfterUpdate.destroy()
        }
      }
    }

    it should "be able to update keys in a single update" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(OtherNamespace, Seq(), keyList.zip(keyList))

          val keyListWithExtraByte = keyList.map(1.toByte +: _)
          val dbAfterUpdate = db.update(OtherNamespace, Seq(), keyList.zip(keyListWithExtraByte))

          keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
            assert(dbAfterUpdate.get(OtherNamespace, key).contains(value))
          }

          dbAfterUpdate.destroy()
        }
      }
    }

    it should "be cleared" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
            .clear

          keyList.foreach { key => assert(db.get(OtherNamespace, key).isEmpty) }

          db.destroy()
        }
      }
    }

    it should "be able to be closed and then continuing using it" in {
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
          db.close()

          val dbAfterClose = createDataSource(path)
          keyList.foreach { key => assert(dbAfterClose.get(OtherNamespace, key).contains(key)) }

          dbAfterClose.destroy()
        }
      }
    }

    it should "be destroyed" in {
      withDir { path =>
        forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
          db.destroy()

          assert(!new File("/tmp/iodbDestroy").exists())

          val dbAfterDestroy = createDataSource(path)
          keyList.foreach { key => assert(dbAfterDestroy.get(OtherNamespace, key).isEmpty) }

          dbAfterDestroy.destroy()
        }
      }
    }

    it should "be able to use multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('o'.toByte)
      forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
        withDir { path =>
          val keyList = unFilteredKeyList.take(KeyNumberLimit)
          val db = createDataSource(path).update(OtherNamespace, Seq(), keyList.zip(keyList))

          val valList1 = keyList.map(1.toByte +: _)
          db.update(OtherNamespace, Seq(), keyList.zip(valList1))

          val valList2 = keyList.map(2.toByte +: _)
          db.update(OtherNamespace2, Seq(), keyList.zip(valList2))

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
  }
  // scalastyle:on

}
