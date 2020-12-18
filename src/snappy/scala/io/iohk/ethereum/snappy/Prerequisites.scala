package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.consensus.StdTestConsensusBuilder
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{DataSourceComponent, RocksDbDataSourceComponent, Storages}
import io.iohk.ethereum.db.dataSource._
import io.iohk.ethereum.db.storage.Namespaces
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.snappy.Config.{DualDB, SingleDB}
import io.iohk.ethereum.snappy.Prerequisites.{RocksDbStorages, Storages}
import monix.execution.Scheduler

object Prerequisites {
  trait NoPruning extends PruningModeComponent {
    val pruningMode: PruningMode = ArchivePruning
  }

  trait Storages extends DataSourceComponent with NoPruning with Storages.DefaultStorages {
    val dataSource: DataSource
  }

  trait RocksDbStorages extends RocksDbDataSourceComponent with Storages
}

class Prerequisites(config: Config) {
  val ec = Scheduler.fixedPool("prerequisites", 4)

  private def rocksDb(dbPath: String): RocksDbDataSource =
    RocksDbDataSource(new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = true
      override val path: String = dbPath
      override val maxThreads: Int = 1
      override val maxOpenFiles: Int = 32
      override val verifyChecksums: Boolean = true
      override val levelCompaction: Boolean = true
      override val blockSize: Long = 16384
      override val blockCacheSize: Long = 33554432
    }, Namespaces.nsSeq)

  private def dbSelection(source: String, dbPath: String): Storages = {
    source match {
      case "rocksdb" => new RocksDbStorages {
        override lazy val dataSource: RocksDbDataSource = rocksDb(dbPath)
      }
    }
  }

  private val sourceStorages: Storages = dbSelection(config.sourceDb, config.sourceDbPath)

  private val targetStorages: Option[Storages] = config.mode match {
    case DualDB => Option(dbSelection(config.sourceDb, config.targetDbPath))

    case SingleDB => None
  }

  val sourceBlockchain = BlockchainImpl(sourceStorages.storages)
  val targetBlockchain: Option[BlockchainImpl] = targetStorages.map(ts => BlockchainImpl(ts.storages))

  private val components = new StdTestConsensusBuilder with SyncConfigBuilder {
    override lazy val vm: VMImpl = new VMImpl
  }

  val blockPreparator = components.consensus.blockPreparator
  val blockchainConfig = components.blockchainConfig
  val syncConfig = components.syncConfig
  val consensus = components.consensus

  val ledger: Ledger = targetBlockchain match {
    case Some(tb) =>
      new LedgerImpl(tb,
        blockchainConfig, syncConfig, consensus, ec)

    case None =>
      new LedgerImpl(sourceBlockchain,
        blockchainConfig, syncConfig, consensus, ec)
  }

  targetBlockchain.foreach { blockchain =>
    val genesisLoader = new GenesisDataLoader(
      blockchain,
      components.blockchainConfig
    )

    genesisLoader.loadGenesisData()
  }
}
