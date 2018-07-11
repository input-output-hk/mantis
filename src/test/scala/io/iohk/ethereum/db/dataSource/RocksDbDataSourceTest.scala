package io.iohk.ethereum.db.dataSource

import java.nio.file.Files

import org.scalatest.FlatSpec

class RocksDbDataSourceTest extends FlatSpec with DataSourceTestBehavior {

  private def createDataSource(path: String): RocksDbDataSource = {
  val dbPath: String = Files.createTempDirectory("temp-test-rocksdb").toAbsolutePath.toString

  RocksDbDataSource(new RocksDbConfig {
    override val createIfMissing: Boolean = true
    override val paranoidChecks: Boolean = true
    override val path: String = dbPath
  })
}

  it should behave like dataSource(createDataSource)
}
