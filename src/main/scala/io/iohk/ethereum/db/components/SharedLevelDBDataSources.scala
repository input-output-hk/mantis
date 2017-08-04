package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource, LevelDBDataSource}
import io.iohk.ethereum.utils.Config

trait SharedLevelDBDataSources extends DataSourcesComponent {

  lazy val dataSource = LevelDBDataSource(Config.Db.LevelDb)

  lazy val dataSources = new DataSources {

    override val blockBodiesDataSource: DataSource = dataSource

    override val blockHeightsHashesDataSource: DataSource = dataSource

    override val blockHeadersDataSource: DataSource = dataSource

    override val evmCodeDataSource: DataSource = dataSource

    override val mptDataSource: DataSource = dataSource

    override val fastSyncStateDataSource: DataSource = dataSource

    override val receiptsDataSource: DataSource = dataSource

    override val totalDifficultyDataSource: DataSource = dataSource

    override val appStateDataSource: DataSource = dataSource

    override val transactionMappingDataSource: DataSource = dataSource

    override val knownNodesDataSource: DataSource = dataSource

    override def closeAll(): Unit = dataSource.close()
  }
}
