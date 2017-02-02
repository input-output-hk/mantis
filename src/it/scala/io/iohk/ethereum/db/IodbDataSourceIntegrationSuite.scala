package io.iohk.ethereum.db

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.ObjectGenerators
import io.iohk.iodb.LSMStore
import java.io.File

class IodbDataSourceIntegrationSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {

  val KeySize: Int = 32 + 1 //Hash size + prefix
  val KeyNumberLimit: Int = 40
  val OtherNamespace: Byte = 'e'.toByte

  def putMultiple(dataSource: DataSource, toInsert: Seq[(Array[Byte], Array[Byte])]): DataSource = {
    toInsert.foldLeft(dataSource){ case (recDB, keyValuePair) =>
      recDB.update(OtherNamespace, Seq(), Seq(keyValuePair))
    }
  }

  test("IodbDataSource insert"){
    forAll(seqByteArrayOfNItemsGen(KeySize)) { unFilteredKeyList: Seq[Array[Byte]] =>
      //create temporary dir
      val dir = File.createTempFile("iodbInsert", "iodbInsert")
      dir.delete()
      dir.mkdir()

      val keyList = unFilteredKeyList.filter(_.length == KeySize).take(KeyNumberLimit)
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