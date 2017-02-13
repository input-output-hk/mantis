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
  }
}
