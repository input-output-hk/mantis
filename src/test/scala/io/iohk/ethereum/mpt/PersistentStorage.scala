package io.iohk.ethereum.mpt

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.db.dataSource._
import io.iohk.ethereum.db.storage.{ ArchiveNodeStorage, NodeStorage }

trait PersistentStorage {

  def withRocksDbNodeStorage(testCode: NodesKeyValueStorage => Unit): Unit = {
    val dbPath = Files.createTempDirectory("rocksdb").toAbsolutePath.toString
    val dataSource = RocksDbDataSource(new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = true
      override val path: String = dbPath
      override val maxThreads: Int = 1
      override val maxOpenFiles: Int = 32
      override val verifyChecksums: Boolean = true
      override val synchronousWrites: Boolean = false
    })

    testExecution(testCode, dbPath, dataSource)
  }

  private def testExecution(testCode: NodesKeyValueStorage => Unit, dbPath: String, dataSource: DataSource): Unit = {
    try {
      testCode(new ArchiveNodeStorage(new NodeStorage(dataSource)))
    } finally {
      val dir = new File(dbPath)
      !dir.exists() || dir.delete()
    }
  }

  def withLevelDbNodeStorage(testCode: NodesKeyValueStorage => Unit): Unit = {
    val dbPath = Files.createTempDirectory("leveldb").toAbsolutePath.toString
    val dataSource = LevelDBDataSource(new LevelDbConfig {
      override val verifyChecksums: Boolean = true
      override val paranoidChecks: Boolean = true
      override val createIfMissing: Boolean = true
      override val path: String = dbPath
      override val maxOpenFiles: Int = 32
    })

    testExecution(testCode, dbPath, dataSource)
  }

}
