package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.consensus.StdTestConsensusBuilder
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{ DataSourcesComponent, SharedLevelDBDataSources, SharedRocksDbDataSources, Storages }
import io.iohk.ethereum.db.dataSource._
import io.iohk.ethereum.db.storage.pruning.{ ArchivePruning, PruningMode }
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{ Ledger, LedgerImpl }
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.snappy.Config.{ DualDB, SingleDB }
import io.iohk.ethereum.snappy.Prerequisites.{ LevelDbStorages, RocksDbStorages, Storages }

object Prerequisites {
  trait NoPruning extends PruningModeComponent {
    val pruningMode: PruningMode = ArchivePruning
  }

  trait Storages extends DataSourcesComponent with NoPruning with Storages.DefaultStorages {
    val dataSource: DataSource
  }

  trait RocksDbStorages extends SharedRocksDbDataSources with Storages

  trait LevelDbStorages extends SharedLevelDBDataSources with Storages

}

class Prerequisites(config: Config) {

  private def levelDb(dbPath: String): LevelDBDataSource =
    LevelDBDataSource (
      new LevelDbConfig {
        override val verifyChecksums: Boolean = true
        override val paranoidChecks: Boolean = true
        override val createIfMissing: Boolean = true
        override val path: String = dbPath
        override val native: Boolean = true
      }
    )

  private def rocksDb(dbPath: String): RocksDbDataSource =
    RocksDbDataSource(new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = true
      override val path: String = dbPath
      override val maxThreads: Int = 1
      override val maxOpenFiles: Int = 32
      override val verifyChecksums: Boolean = true
      override val synchronousWrites: Boolean = false
    })

  private def dbSelection(source: String, dbPath: String): Storages = {
    source match {
      case "rocksdb" => new RocksDbStorages {
        override lazy val dataSource: RocksDbDataSource = rocksDb(dbPath)
      }

      case "leveldb" => new LevelDbStorages {
        override lazy val dataSource: LevelDBDataSource = levelDb(dbPath)
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

  val ledger: Ledger = targetBlockchain match {
    case Some(tb) =>
      new LedgerImpl(tb,
        components.blockchainConfig, components.syncConfig, components.consensus)

    case None =>
      new LedgerImpl(sourceBlockchain,
        components.blockchainConfig, components.syncConfig, components.consensus)
  }

  targetBlockchain.foreach { blockchain =>
    val genesisLoader = new GenesisDataLoader(
      blockchain,
      components.blockchainConfig
    )

    genesisLoader.loadGenesisData()
  }
}
