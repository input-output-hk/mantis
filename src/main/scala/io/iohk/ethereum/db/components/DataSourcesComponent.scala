package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.DataSource

trait DataSourcesComponent {

  val dataSources: DataSources

  trait DataSources {

    def evmCodeDataSource: DataSource

    def mptDataSource: DataSource

    def receiptsDataSource: DataSource

    def blockHeadersDataSource: DataSource

    def blockBodiesDataSource: DataSource

    def blockHeightsHashesDataSource: DataSource

    def totalDifficultyDataSource: DataSource

    def appStateDataSource: DataSource

    def closeAll: Unit

  }

}
