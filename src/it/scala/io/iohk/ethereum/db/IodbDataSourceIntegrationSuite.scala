package io.iohk.ethereum.db

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.ObjectGenerators
import io.iohk.iodb.LSMStore
import java.io.File

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource}
import org.scalacheck.Gen

import scala.util.{Random, Try}

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
      //create temporary dir
      val dir = File.createTempFile("iodbInsert", "iodbInsert")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = updateInSeparateCalls(
        dataSource = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize)),
        toUpsert = keyList.zip(keyList)
      )
      keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }
    }
  }

  test("IodbDataSource insert keys in a single update"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbInsert", "iodbInsert")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize))
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }
    }
  }

  test("IodbDataSource update keys in separate updates"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbUpdate", "iodbUpdate")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize))
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)
      val dbAfterUpdate = updateInSeparateCalls(db, keyList.zip(keyListWithExtraByte))

      keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
        assert(dbAfterUpdate.get(OtherNamespace, key).contains(value)) }
    }
  }

  test("IodbDataSource update keys in a single update"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbUpdate", "iodbUpdate")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize))
        .update(OtherNamespace, Seq(), keyList.zip(keyList))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)
      val dbAfterUpdate = db.update(OtherNamespace, Seq(), keyList.zip(keyListWithExtraByte))

      keyList.zip(keyListWithExtraByte).foreach { case (key, value) =>
        assert(dbAfterUpdate.get(OtherNamespace, key).contains(value))
      }
    }
  }

  test("IodbDataSource insert/update with invalid length") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbInvalidLength", "iodbInvalidLength")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val (keysLeft, keysToInsert) = Random.shuffle(keyList).splitAt(Gen.choose(0, keyList.size/2).sample.get)
      val db = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize))
        .update(OtherNamespace, Seq(), keysToInsert.zip(keysToInsert))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)

      val invalidKeyList = keyList.map{ key =>
        val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map( i => 1.toByte )
        suffixOfRandomLength ++ key
      }

      invalidKeyList.foreach { key => assert( Try{db.update(OtherNamespace, Seq(), Seq(key->key))}.isFailure) }
    }
  }

  test("IodbDataSource get with invalid length") {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbInvalidLength", "iodbInvalidLength")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val (keysLeft, keysToInsert) = Random.shuffle(keyList).splitAt(Gen.choose(0, keyList.size/2).sample.get)
      val db = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize))
        .update(OtherNamespace, Seq(), keysToInsert.zip(keysToInsert))

      val keyListWithExtraByte = keyList.map(1.toByte +: _)

      val invalidKeyList = keyList.map{ key =>
        val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map( i => 1.toByte )
        suffixOfRandomLength ++ key
      }

      invalidKeyList.foreach { key => assert( db.get(OtherNamespace, key).isEmpty) }
    }
  }
}