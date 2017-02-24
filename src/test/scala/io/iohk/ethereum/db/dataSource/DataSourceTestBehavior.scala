package io.iohk.ethereum.db.dataSource

import java.io.File
import java.nio.file.Files

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

trait DataSourceTestBehavior
  extends PropertyChecks
    with ObjectGenerators {

  this: FlatSpec =>

  val KeySizeWithoutPrefix: Int = 32
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('e'.toByte)

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
      val someByteString = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)
        dataSource.update(OtherNamespace, Seq(), Seq(someByteString -> someByteString))

        dataSource.get(OtherNamespace, someByteString) match {
          case Some(b) if b == someByteString => succeed
          case _ => fail
        }

        dataSource.destroy()
      }
    }

    it should "allow to remove keys" in {
      val key1 = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      val key2 = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(OtherNamespace, Seq(), Seq(key1 -> key1, key2 -> key2))

        assert(dataSource.get(OtherNamespace, key1).isDefined)
        assert(dataSource.get(OtherNamespace, key2).isDefined)

        dataSource.update(OtherNamespace, Seq(key1), Seq())

        assert(dataSource.get(OtherNamespace, key1).isEmpty)
        assert(dataSource.get(OtherNamespace, key2).isDefined)

        dataSource.destroy()
      }
    }

    it should "remove all keys after clear" in {
      val someByteString = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(OtherNamespace, Seq(), Seq(someByteString -> someByteString))

        assert(dataSource.get(OtherNamespace, someByteString).isDefined)

        val newDataSource = dataSource.clear

        assert(newDataSource.get(OtherNamespace, someByteString).isEmpty)

        dataSource.destroy()
      }
    }

    it should "allow using multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('o'.toByte)
      val someByteString = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      val someValue1 = 1.toByte +: someByteString
      val someValue2 = 2.toByte +: someByteString
      withDir { path =>
        val dataSource = createDataSource(path)

        //Insertion
        dataSource.update(OtherNamespace, Seq(), Seq(someByteString -> someValue1))
        dataSource.update(OtherNamespace2, Seq(), Seq(someByteString -> someValue2))

        assert(dataSource.get(OtherNamespace, someByteString).contains(someValue1))
        assert(dataSource.get(OtherNamespace2, someByteString).contains(someValue2))

        //Removal
        dataSource.update(OtherNamespace2, Seq(someByteString), Seq.empty)

        assert(dataSource.get(OtherNamespace, someByteString).contains(someValue1))
        assert(dataSource.get(OtherNamespace2, someByteString).isEmpty)

        dataSource.destroy()
      }
    }
  }
  // scalastyle:on


}
