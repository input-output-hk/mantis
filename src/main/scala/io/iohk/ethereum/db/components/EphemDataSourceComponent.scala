package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.EphemDataSource

trait EphemDataSourceComponent extends DataSourceComponent {
  val dataSource: EphemDataSource = EphemDataSource()
}
