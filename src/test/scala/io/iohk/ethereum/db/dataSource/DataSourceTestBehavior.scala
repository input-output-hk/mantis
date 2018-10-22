package io.iohk.ethereum.db.dataSource

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

trait DataSourceTestBehavior
  extends PropertyChecks
    with ObjectGenerators {

  this: FlatSpec =>

  val KeySizeWithoutPrefix: Int = 32
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)

  def withDir(testCode: String => Any): Unit = {
    val path = Files.createTempDirectory("testdb").getFileName.toString
    try {
      testCode(path)
    } finally {
      val dir = new File(path)
      assert(!dir.exists() || dir.delete(), "File deletion failed")
    }
  }

  // scalastyle:off
  def dataSource(createDataSource: => String => DataSource): Unit = {
    it should "be able to insert and retrieve stored keys" in {
      val array = byteArrayOfNItemsGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)
        dataSource.update(OtherNamespace, Seq(), Seq(array -> array))

        dataSource.get(OtherNamespace, array) match {
          case Some(b) if b sameElements array => succeed
          case _ => fail
        }

        dataSource.destroy()
      }
    }

    it should "allow to remove keys" in {
      val array1 = byteArrayOfNItemsGen(KeySizeWithoutPrefix).sample.get
      val array2 = byteArrayOfNItemsGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(OtherNamespace, Seq(), Seq(array1 -> array1, array2 -> array2))

        assert(dataSource.get(OtherNamespace, array1).isDefined)
        assert(dataSource.get(OtherNamespace, array2).isDefined)

        dataSource.update(OtherNamespace, Seq(array1), Seq())

        assert(dataSource.get(OtherNamespace, array1).isEmpty)
        assert(dataSource.get(OtherNamespace, array2).isDefined)

        dataSource.destroy()
      }
    }

    it should "remove all keys after clear" in {
      val someByteArray = byteArrayOfNItemsGen(KeySizeWithoutPrefix).sample.get

      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(OtherNamespace, Seq(), Seq(someByteArray -> someByteArray))

        assert(dataSource.get(OtherNamespace, someByteArray).isDefined)

        val newDataSource = dataSource.clear

        assert(newDataSource.get(OtherNamespace, someByteArray).isEmpty)

        dataSource.destroy()
      }
    }

    it should "allow using multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
      val someByteArray = byteArrayOfNItemsGen(KeySizeWithoutPrefix).sample.get
      val someValue1 = 1.toByte +: someByteArray
      val someValue2 = 2.toByte +: someByteArray
      withDir { path =>
        val dataSource = createDataSource(path)

        //Insertion
        dataSource.update(OtherNamespace, Seq(), Seq(someByteArray -> someValue1))
        dataSource.update(OtherNamespace2, Seq(), Seq(someByteArray -> someValue2))

        assert(dataSource.get(OtherNamespace, someByteArray).forall(_ sameElements someValue1))
        assert(dataSource.get(OtherNamespace2, someByteArray).forall(_ sameElements someValue2))

        //Removal
        dataSource.update(OtherNamespace2, Seq(someByteArray), Nil)

        assert(dataSource.get(OtherNamespace, someByteArray).forall(_ sameElements someValue1))
        assert(dataSource.get(OtherNamespace2, someByteArray).isEmpty)

        dataSource.destroy()
      }
    }
  }
  // scalastyle:on

}
