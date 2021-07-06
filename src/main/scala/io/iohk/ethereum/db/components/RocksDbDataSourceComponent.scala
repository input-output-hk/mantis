package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.RocksDbDataSource
import io.iohk.ethereum.db.storage.Namespaces
import io.iohk.ethereum.utils.Config

trait RocksDbDataSourceComponent extends DataSourceComponent {

  lazy val dataSource: RocksDbDataSource = RocksDbDataSource(Config.Db.RocksDb, Namespaces.nsSeq)

}
