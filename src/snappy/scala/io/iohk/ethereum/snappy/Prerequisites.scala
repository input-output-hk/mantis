package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, LedgerBuilder, ValidatorsBuilder}
import io.iohk.ethereum.utils.Config.DbConfig

class Prerequisites(config: Config) {

  trait NoPruning extends PruningModeComponent {
    val pruningMode = ArchivePruning
  }

  private def levelDb(dbPath: String): LevelDBDataSource =
    LevelDBDataSource (
      new LevelDbConfig {
        val verifyChecksums: Boolean = true
        val paranoidChecks: Boolean = true
        val createIfMissing: Boolean = true
        val path: String = dbPath
      }
    )

  val sourceStorages = new SharedLevelDBDataSources with NoPruning with Storages.DefaultStorages {
    override lazy val dataSource = levelDb(config.sourceDbPath)
  }

  val targetStorages = new SharedLevelDBDataSources with NoPruning with Storages.DefaultStorages {
    override lazy val dataSource = levelDb(config.targetDbPath)
  }

  val sourceBlockchain = BlockchainImpl(sourceStorages.storages)
  val targetBlockchain = BlockchainImpl(targetStorages.storages)

  private val components =
    new LedgerBuilder
      with ValidatorsBuilder
      with BlockchainConfigBuilder


  val ledger = components.ledger

  val validators = components.validators

  val genesisLoader = new GenesisDataLoader(
    targetStorages.dataSource,
    BlockchainImpl(targetStorages.storages),
    ArchivePruning,
    components.blockchainConfig,
    new DbConfig { val batchSize: Int = 1000 }
  )

  genesisLoader.loadGenesisData()
}
