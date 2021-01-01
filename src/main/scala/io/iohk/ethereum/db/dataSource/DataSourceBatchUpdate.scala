package io.iohk.ethereum.db.dataSource

import scala.collection.immutable.ArraySeq

case class DataSourceBatchUpdate(dataSource: DataSource, updates: Array[DataUpdate] = Array.empty) {

  def and(that: DataSourceBatchUpdate): DataSourceBatchUpdate = {
    require(
      this.dataSource eq that.dataSource,
      "Transactional storage updates must be performed on the same data source"
    )
    DataSourceBatchUpdate(dataSource, this.updates ++ that.updates)
  }

  def commit(): Unit = {
    dataSource.update(ArraySeq.unsafeWrapArray(updates))
  }

}
