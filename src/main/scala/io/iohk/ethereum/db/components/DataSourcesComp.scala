package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.dataSource.DataSource

trait DataSourcesComp {

  val dataSources: DataSources

  trait DataSources {

    val evmCodeStorage: DataSource

    val mptDataSource: DataSource

    val receiptsDataSource: DataSource

    val blockHeadersDataSource: DataSource

    val blockBodiesDataSource: DataSource

    val blockHeightsHashesDataSource: DataSource

    def closeAll: Unit

  }

}
