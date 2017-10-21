package io.iohk.ethereum.mpt

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}

trait PersistentStorage {
  def withNodeStorage(testCode: NodesKeyValueStorage => Unit): Unit = {
    val dbPath = Files.createTempDirectory("testdb").toAbsolutePath.toString
    val dataSource = LevelDBDataSource(new LevelDbConfig {
      override val verifyChecksums: Boolean = true
      override val paranoidChecks: Boolean = true
      override val createIfMissing: Boolean = true
      override val path: String = dbPath
    })

    try {
      testCode(new ArchiveNodeStorage(new NodeStorage(dataSource)))
    } finally {
      val dir = new File(dbPath)
      !dir.exists() || dir.delete()
    }
  }
}
