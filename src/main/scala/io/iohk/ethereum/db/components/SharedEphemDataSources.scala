package io.iohk.ethereum.db.components
import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource}

trait SharedEphemDataSources extends DataSourcesComp {

  val ephemDataSource = EphemDataSource()

  val dataSources = new DataSources {

    override def evmCodeStorage: DataSource = ephemDataSource

    override def mptDataSource: DataSource = ephemDataSource

    override def receiptsDataSource: DataSource = ephemDataSource

    override def blockBodiesDataSource: DataSource = ephemDataSource

    override def blockHeightsHashesDataSource: DataSource = ephemDataSource

    override def blockHeadersDataSource: DataSource = ephemDataSource

    override def closeAll: Unit = ()
  }

}
