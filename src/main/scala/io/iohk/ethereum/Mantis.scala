package io.iohk.ethereum

import java.util.logging.LogManager

import org.rocksdb

import io.iohk.ethereum.nodebuilder.StdNode
import io.iohk.ethereum.nodebuilder.TestNode
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Logger

object Mantis extends Logger {
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager().reset(); // disable java.util.logging, ie. in legacy parts of jupnp

    val node =
      if (Config.testmode) {
        log.info("Starting Mantis in test mode")
        deleteRocksDBFiles()
        new TestNode
      } else new StdNode

    log.info("Mantis app {}", Config.clientVersion)
    log.info("Using network {}", Config.blockchains.network)

    node.start()
  }

  private def deleteRocksDBFiles(): Unit = {
    log.warn("Deleting previous database {}", Config.Db.RocksDb.path)
    rocksdb.RocksDB.destroyDB(Config.Db.RocksDb.path, new rocksdb.Options())
  }
}
