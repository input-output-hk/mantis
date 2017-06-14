package io.iohk.ethereum.db.components
import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource}

trait SharedEphemDataSources extends DataSourcesComponent {

  val ephemDataSource = EphemDataSource()

  val dataSources = new DataSources {

    override val evmCodeDataSource: DataSource = ephemDataSource

    override val mptDataSource: DataSource = ephemDataSource

    override val fastSyncStateDataSource: DataSource = ephemDataSource

    override val receiptsDataSource: DataSource = ephemDataSource

    override val blockBodiesDataSource: DataSource = ephemDataSource

    override val blockHeightsHashesDataSource: DataSource = ephemDataSource

    override val blockHeadersDataSource: DataSource = ephemDataSource

    override val totalDifficultyDataSource: DataSource = ephemDataSource

    override val appStateDataSource: DataSource = ephemDataSource

    override val transactionMappingDataSource: DataSource = ephemDataSource

    override def closeAll(): Unit = ()
  }

}
