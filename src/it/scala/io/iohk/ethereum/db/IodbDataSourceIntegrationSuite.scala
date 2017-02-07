package io.iohk.ethereum.db

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.ObjectGenerators
import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource}
import org.scalacheck.Gen
import scala.util.Try
import java.io.File

//FIXME: Add IodbDataSource delete tests (currently not implemented as they failed in previous IODB implementation)
class IodbDataSourceIntegrationSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {

  val KeySizeWithoutPrefix: Int = 32
  val KeySize: Int = KeySizeWithoutPrefix + 1 //Hash size + prefix
  val KeyNumberLimit: Int = 40
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('e'.toByte)

  val MaxIncreaseInLength = 10

  def updateInSeparateCalls(dataSource: DataSource, toUpsert: Seq[(ByteString, ByteString)]): DataSource = {
    toUpsert.foldLeft(dataSource){ case (recDB, keyValuePair) =>
      recDB.update(OtherNamespace, Seq(), Seq(keyValuePair))
    }
  }

  test("IodbDataSource insert keys in separate updates"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = updateInSeparateCalls(
        dataSource = IodbDataSource(path = "/tmp/iodbInsert", keySize = KeySize, createNew = true),
        toUpsert = keyList.zip(keyList)
      )
      keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }

      db.destroy()
    }
  }

  test("IodbDataSource insert keys in a single update"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbInsert", keySize = KeySize, createNew = true)
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }

      db.destroy()
    }
  }

  test("IodbDataSource update keys in separate updates"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbUpdate", keySize = KeySize, createNew = true)
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)
      val dbAfterUpdate = updateInSeparateCalls(db, keyList.zip(keyListWithExtraByte))

      keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
        assert(dbAfterUpdate.get(OtherNamespace, key).contains(value)) }

      dbAfterUpdate.destroy()
    }
  }

  test("IodbDataSource update keys in a single update"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbUpdate", keySize = KeySize, createNew = true)
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)
      val dbAfterUpdate = db.update(OtherNamespace, Seq(), keyList.zip(keyListWithExtraByte))

      keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
        assert(dbAfterUpdate.get(OtherNamespace, key).contains(value))
      }

      dbAfterUpdate.destroy()
    }
  }

  test("IodbDataSource insert/update with invalid length") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val (keysLeft, keysToInsert) = keyList.splitAt(Gen.choose(0, keyList.size/2).sample.get)
      val db = IodbDataSource(path = "/tmp/iodbInvalidLength", keySize = KeySize, createNew = true)
        .update(OtherNamespace, Seq(), keysToInsert.zip(keysToInsert))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)

      val invalidKeyList = keyList.map{ key =>
        val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map( i => 1.toByte )
        suffixOfRandomLength ++ key
      }

      invalidKeyList.foreach { key => assert( Try{db.update(OtherNamespace, Seq(), Seq(key->key))}.isFailure) }

      db.destroy()
    }
  }

  test("IodbDataSource get with invalid length") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val (keysLeft, keysToInsert) = keyList.splitAt(Gen.choose(0, keyList.size / 2).sample.get)
      val db = IodbDataSource(path = "/tmp/iodbInvalidLength", keySize = KeySize, createNew = true)
        .update(OtherNamespace, Seq(), keysToInsert.zip(keysToInsert))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)

      val invalidKeyList = keyList.map { key =>
        val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map(i => 1.toByte)
        suffixOfRandomLength ++ key
      }

      invalidKeyList.foreach { key => assert( Try{db.get(OtherNamespace, key)}.isFailure) }

      db.destroy()
    }
  }

  test("IodbDataSource clear"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbClean", keySize = KeySize, createNew = true)
        .update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
        .clear

      keyList.foreach { key => assert(db.get(OtherNamespace, key).isEmpty) }

      db.destroy()
    }
  }

  test("IodbDataSource close and clear before using it again") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbClose", keySize = KeySize, createNew = true)
        .update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
      db.close()

      val dbAfterClose = IodbDataSource(path = "/tmp/iodbClose", keySize = KeySize, createNew = true)
      keyList.foreach { key => assert(dbAfterClose.get(OtherNamespace, key).isEmpty) }

      dbAfterClose.destroy()
    }
  }

  test("IodbDataSource close and then continuing using it") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbClose", keySize = KeySize, createNew = true)
        .update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
      db.close()

      val dbAfterClose = IodbDataSource(path = "/tmp/iodbClose", keySize = KeySize, createNew = false)
      keyList.foreach { key => assert(dbAfterClose.get(OtherNamespace, key).contains(key)) }

      dbAfterClose.destroy()
    }
  }

  test("IodbDataSource destroy") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = IodbDataSource(path = "/tmp/iodbDestroy", keySize = KeySize, createNew = true)
        .update(namespace = OtherNamespace, toRemove = Seq(), toUpsert = keyList.zip(keyList))
      db.destroy()

      assert(!new File("/tmp/iodbDestroy").exists())

      val dbAfterDestroy = IodbDataSource(path = "/tmp/iodbDestroy", keySize = KeySize, createNew = true)
      keyList.foreach { key => assert(dbAfterDestroy.get(OtherNamespace, key).isEmpty) }

      dbAfterDestroy.destroy()
    }
  }
}