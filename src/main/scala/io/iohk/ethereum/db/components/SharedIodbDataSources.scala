package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource, IodbDataSource}
import io.iohk.ethereum.utils.Config

trait SharedIodbDataSources extends DataSourcesComponent {

  val dataSource = IodbDataSource(Config.Db.Iodb.path, IodbDataSource.KeySize)

  override val dataSources = new DataSources {

    override val blockBodiesDataSource: DataSource = dataSource

    override val blockHeightsHashesDataSource: DataSource = dataSource

    override val blockHeadersDataSource: DataSource = dataSource

    override val evmCodeStorage: DataSource = dataSource

    override val mptDataSource: DataSource = dataSource

    override val receiptsDataSource: DataSource = dataSource

    override def closeAll: Unit = dataSource.close()
  }
}
