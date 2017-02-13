package io.iohk.ethereum.db.dataSource

import org.scalatest.FlatSpec

class IodbDataSourceTest extends FlatSpec with DataSourceTestBehavior {

  private def createDataSource(path: String) = IodbDataSource(path, KeySizeWithoutPrefix + 1)

  it should behave like dataSource(createDataSource)
}
