package io.iohk.ethereum.db

import org.scalatest.flatspec.AnyFlatSpec

import io.iohk.ethereum.db.dataSource.RocksDbConfig
import io.iohk.ethereum.db.dataSource.RocksDbDataSource
import io.iohk.ethereum.db.storage.Namespaces

class RocksDbDataSourceIntegrationSuite extends AnyFlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(dataSourcePath: String) = RocksDbDataSource(
    new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = true
      override val path: String = dataSourcePath
      override val maxThreads: Int = 1
      override val maxOpenFiles: Int = 32
      override val verifyChecksums: Boolean = true
      override val levelCompaction: Boolean = true
      override val blockSize: Long = 16384
      override val blockCacheSize: Long = 33554432
    },
    Namespaces.nsSeq
  )

  (it should behave).like(dataSource(createDataSource))
}
