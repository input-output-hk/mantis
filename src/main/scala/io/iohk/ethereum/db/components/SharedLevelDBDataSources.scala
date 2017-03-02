package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource, LevelDBDataSource}
import io.iohk.ethereum.utils.Config

trait SharedLevelDBDataSources extends DataSourcesComponent {

  val dataSource = LevelDBDataSource(Config.Db.LevelDb)

  val dataSources = new DataSources {

    override val blockBodiesDataSource: DataSource = dataSource

    override val blockHeightsHashesDataSource: DataSource = dataSource

    override val blockHeadersDataSource: DataSource = dataSource

    override val evmCodeStorage: DataSource = dataSource

    override val mptDataSource: DataSource = dataSource

    override val receiptsDataSource: DataSource = dataSource

    override val totalDifficultyDataSource: DataSource = dataSource

    override def closeAll: Unit = dataSource.close()
  }
}
