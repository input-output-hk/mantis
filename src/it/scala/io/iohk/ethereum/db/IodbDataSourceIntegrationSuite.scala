package io.iohk.ethereum.db

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.ObjectGenerators
import io.iohk.iodb.LSMStore
import java.io.File

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource}

class IodbDataSourceIntegrationSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {

  val KeySizeWithoutPrefix: Int = 32
  val KeySize: Int = KeySizeWithoutPrefix + 1 //Hash size + prefix
  val KeyNumberLimit: Int = 40
  val OtherNamespace: Byte = 'e'.toByte

  def putMultiple(dataSource: DataSource, toInsert: Seq[(ByteString, ByteString)]): DataSource = {
    toInsert.foldLeft(dataSource){ case (recDB, keyValuePair) =>
      recDB.update(OtherNamespace, Seq(), Seq(keyValuePair))
    }
  }

  test("IodbDataSource insert"){
    forAll(seqByteStringOfNItemsGen(KeySizeWithoutPrefix)) { unFilteredKeyList: Seq[ByteString] =>
      //create temporary dir
      val dir = File.createTempFile("iodbInsert", "iodbInsert")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.take(KeyNumberLimit)
      val db = putMultiple(
        dataSource = new IodbDataSource(new LSMStore(dir = dir, keySize = KeySize)),
        toInsert = keyList.zip(keyList)
      )
      keyList.foreach { key =>
        val obtained = db.get(OtherNamespace, key)
        assert(obtained.isDefined)
        assert(obtained.get sameElements key)
      }
    }
  }
}