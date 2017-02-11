package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.DataSource

trait DataSourcesComp {

  val dataSources: DataSources

  trait DataSources {

    def evmCodeStorage: DataSource

    def mptDataSource: DataSource

    def receiptsDataSource: DataSource

    def blockHeadersDataSource: DataSource

    def blockBodiesDataSource: DataSource

    def blockHeightsHashesDataSource: DataSource

    def closeAll: Unit

  }

}
