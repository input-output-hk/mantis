package io.iohk.ethereum.db

import io.iohk.ethereum.db.dataSource.LevelDBDataSource
import org.scalatest.FlatSpec

class LevelDBDataSourceIntegrationSuite extends FlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(path: String) = LevelDBDataSource(path)

  it should behave like dataSource(createDataSource)
}