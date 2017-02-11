package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource, IodbDataSource}
import io.iohk.ethereum.utils.Config

trait SharedIodbDataSources extends DataSourcesComp {

  val dataSource = IodbDataSource(Config.Db.Iodb.path, IodbDataSource.KeySize)

  override val dataSources = new DataSources {

    override def blockBodiesDataSource: DataSource = dataSource

    override def blockHeightsHashesDataSource: DataSource = dataSource

    override def blockHeadersDataSource: DataSource = dataSource

    override def evmCodeStorage: DataSource = dataSource

    override def mptDataSource: DataSource = dataSource

    override def receiptsDataSource: DataSource = dataSource

    override def closeAll: Unit = dataSource.close()
  }
}
