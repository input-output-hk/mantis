package io.iohk.ethereum.db

import io.iohk.ethereum.db.dataSource.{ RocksDbConfig, RocksDbDataSource }
import org.scalatest.FlatSpec

class RocksDbDataSourceIntegrationSuite extends FlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(dataSourcePath: String) = RocksDbDataSource(new RocksDbConfig {
    override val createIfMissing: Boolean = true
    override val paranoidChecks: Boolean = true
    override val path: String = dataSourcePath
    override val maxThreads: Int = 1
    override val maxOpenFiles: Int = 32
    override val verifyChecksums: Boolean = true
    override val synchronousWrites: Boolean = false
  })

  it should behave like dataSource(createDataSource)
}

