package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.consensus.StdTestConsensusBuilder
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{ SharedRocksDbDataSource, Storages }
import io.iohk.ethereum.db.dataSource.{ RocksDbConfig, RocksDbDataSource }
import io.iohk.ethereum.db.storage.pruning
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{ Ledger, LedgerImpl }
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.snappy.Config.{ DualDB, SingleDB }
import io.iohk.ethereum.snappy.Prerequisites._


object Prerequisites {
  trait NoPruning extends PruningModeComponent {
    val pruningMode: pruning.ArchivePruning.type = ArchivePruning
  }

  trait RocksDbStorages extends SharedRocksDbDataSource with NoPruning with Storages.DefaultStorages
}

class Prerequisites(config: Config) {

  private def rocksDb(dbPath: String): RocksDbDataSource =
    RocksDbDataSource(new RocksDbConfig {
      override val createIfMissing: Boolean = true
      override val paranoidChecks: Boolean = true
      override val path: String = dbPath
    })

  private val sourceStorages: RocksDbStorages = new RocksDbStorages {
    override lazy val dataSource: RocksDbDataSource = rocksDb(config.sourceDbPath)
  }

  private val targetStorages: Option[RocksDbStorages] = config.mode match {
    case DualDB =>
      Some(new RocksDbStorages {
        override lazy val dataSource: RocksDbDataSource = rocksDb(config.targetDbPath)
      })

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
