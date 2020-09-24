package io.iohk.ethereum.db.dataSource

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait DataSourceTestBehavior extends ScalaCheckPropertyChecks with ObjectGenerators {
  this: AnyFlatSpec =>

  val KeySizeWithoutPrefix: Int = 32
  val OtherNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)

  def prepareUpdate(
      namespace: Namespace = OtherNamespace,
      toRemove: Seq[Key] = Nil,
      toUpsert: Seq[(Key, Value)] = Nil
  ): Seq[DataSourceUpdate] =
    Seq(DataSourceUpdate(namespace, toRemove, toUpsert))

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
        dataSource.update(prepareUpdate(toUpsert = Seq(someByteString -> someByteString)))

        dataSource.get(OtherNamespace, someByteString) match {
          case Some(b) if b == someByteString => succeed
          case _ => fail()
        }

        dataSource.destroy()
      }
    }

    it should "allow to remove keys" in {
      val key1 = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      val key2 = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(prepareUpdate(toUpsert = Seq(key1 -> key1, key2 -> key2)))

        assert(dataSource.get(OtherNamespace, key1).isDefined)
        assert(dataSource.get(OtherNamespace, key2).isDefined)

        dataSource.update(prepareUpdate(toRemove = Seq(key1)))

        assert(dataSource.get(OtherNamespace, key1).isEmpty)
        assert(dataSource.get(OtherNamespace, key2).isDefined)

        dataSource.destroy()
      }
    }

    it should "remove all keys after clear" in {
      val someByteString = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      withDir { path =>
        val dataSource = createDataSource(path)

        dataSource.update(prepareUpdate(toUpsert = Seq(someByteString -> someByteString)))

        assert(dataSource.get(OtherNamespace, someByteString).isDefined)

        dataSource.clear()

        assert(dataSource.get(OtherNamespace, someByteString).isEmpty)

        dataSource.destroy()
      }
    }

    it should "allow using multiple namespaces with the same key" in {
      val OtherNamespace2: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
      val someByteString = byteStringOfLengthNGen(KeySizeWithoutPrefix).sample.get
      val someValue1 = 1.toByte +: someByteString
      val someValue2 = 2.toByte +: someByteString
      withDir { path =>
        val dataSource = createDataSource(path)

        //Insertion
        dataSource.update(prepareUpdate(namespace = OtherNamespace, toUpsert = Seq(someByteString -> someValue1)))
        dataSource.update(prepareUpdate(namespace = OtherNamespace2, toUpsert = Seq(someByteString -> someValue2)))

        assert(dataSource.get(OtherNamespace, someByteString).contains(someValue1))
        assert(dataSource.get(OtherNamespace2, someByteString).contains(someValue2))

        //Removal
        dataSource.update(prepareUpdate(namespace = OtherNamespace2, toRemove = Seq(someByteString)))

        assert(dataSource.get(OtherNamespace, someByteString).contains(someValue1))
        assert(dataSource.get(OtherNamespace2, someByteString).isEmpty)

        dataSource.destroy()
      }
    }
  }
  // scalastyle:on

}
