package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.IodbDataSource
import org.scalacheck.Gen
import org.scalatest.FlatSpec

import scala.util.Try

class IodbDataSourceIntegrationSuite extends FlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(path: String) = IodbDataSource(path, KeySize)

  it should behave like dataSource(createDataSource)

  it should "error when insert/update with invalid length" in {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      withDir { path =>
        val keyList = unFilteredKeyList.take(KeyNumberLimit)
        val keysToInsert = keyList.take(keyList.size/2)
        val db = createDataSource(path).update(OtherNamespace, Seq(), keysToInsert.zip(keysToInsert))

        val invalidKeyList = keyList.map{ key =>
          val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map(_ => 1.toByte )
          suffixOfRandomLength ++ key
        }

        invalidKeyList.foreach { key => assert( Try{db.update(OtherNamespace, Seq(), Seq(key->key))}.isFailure) }

        db.destroy()
      }
    }
  }

  it should "error get with invalid length" in {
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      withDir { path =>
        val keyList = unFilteredKeyList.take(KeyNumberLimit)
        val db = createDataSource(path).update(OtherNamespace, Seq(), keyList.zip(keyList))

        val invalidKeyList = keyList.map { key =>
          val suffixOfRandomLength = (0 until Gen.choose(1, MaxIncreaseInLength).sample.get).map(_ => 1.toByte)
          suffixOfRandomLength ++ key
        }

        invalidKeyList.foreach { key => assert( Try{db.get(OtherNamespace, key)}.isFailure) }

        db.destroy()
      }
    }
  }
}
