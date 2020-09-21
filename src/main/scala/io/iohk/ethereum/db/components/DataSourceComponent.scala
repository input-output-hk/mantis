package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.DataSource

trait DataSourceComponent {
  val dataSource: DataSource
}
