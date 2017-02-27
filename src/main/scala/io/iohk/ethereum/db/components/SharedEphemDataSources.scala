package io.iohk.ethereum.db.components
import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource}

trait SharedEphemDataSources extends DataSourcesComponent {

  val ephemDataSource = EphemDataSource()

  val dataSources = new DataSources {

    override val evmCodeStorage: DataSource = ephemDataSource

    override val mptDataSource: DataSource = ephemDataSource

    override val receiptsDataSource: DataSource = ephemDataSource

    override val blockBodiesDataSource: DataSource = ephemDataSource

    override val blockHeightsHashesDataSource: DataSource = ephemDataSource

    override val blockHeadersDataSource: DataSource = ephemDataSource

    override def closeAll: Unit = ()
  }

}
