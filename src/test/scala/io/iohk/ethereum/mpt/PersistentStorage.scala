package io.iohk.ethereum.mpt

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.db.dataSource._
import io.iohk.ethereum.db.storage._

trait PersistentStorage {

  def withRocksDbNodeStorage(testCode: MptStorage => Unit): Unit = {
    val dbPath = Files.createTempDirectory("rocksdb").toAbsolutePath.toString
    val dataSource = RocksDbDataSource(
      new RocksDbConfig {
        override val createIfMissing: Boolean = true
        override val paranoidChecks: Boolean = true
        override val path: String = dbPath
        override val maxThreads: Int = 1
        override val maxOpenFiles: Int = 32
        override val verifyChecksums: Boolean = true
        override val levelCompaction: Boolean = true
        override val blockSize: Long = 16384
        override val blockCacheSize: Long = 33554432
      },
      Namespaces.nsSeq
    )

    testExecution(testCode, dbPath, dataSource)
    dataSource.destroy()
  }

  private def testExecution(testCode: MptStorage => Unit, dbPath: String, dataSource: DataSource): Unit =
    try testCode(new SerializingMptStorage(new ArchiveNodeStorage(new NodeStorage(dataSource))))
    finally {
      val dir = new File(dbPath)
      !dir.exists() || dir.delete()
    }

}
