package io.iohk.ethereum.db

import io.iohk.ethereum.db.dataSource.{ RocksDbConfig, RocksDbDataSource }
import org.scalatest.FlatSpec

class RocksDbDataSourceIntegrationSuite extends FlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(dataSourcePath: String) = RocksDbDataSource(new RocksDbConfig {
    override val createIfMissing: Boolean = true
    override val paranoidChecks: Boolean = true
    override val path: String = dataSourcePath
  })

  it should behave like dataSource(createDataSource)
}

