package io.iohk.ethereum.db.dataSource

import java.nio.file.Files

import org.scalatest.FlatSpec

class LevelDbDataSourceTest extends FlatSpec with DataSourceTestBehavior {

  private def createDataSource(path: String): LevelDBDataSource = {
    val dbPath: String = Files.createTempDirectory("temp-test-leveldb").toAbsolutePath.toString

    LevelDBDataSource(new LevelDbConfig {
      override val verifyChecksums: Boolean = true
      override val paranoidChecks: Boolean = true
      override val createIfMissing: Boolean = true
      override val path: String = dbPath
      override val native: Boolean = true
      override val maxOpenFiles: Int = 32
    })
  }

  it should behave like dataSource(createDataSource)
}

