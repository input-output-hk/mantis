package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.consensus.{ConsensusBuilder, ConsensusConfigBuilder}
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, ShutdownHookBuilder, SyncConfigBuilder, ValidatorsBuilder}
import io.iohk.ethereum.snappy.Config.{DualDB, SingleDB}
import io.iohk.ethereum.snappy.Prerequisites._
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.vm.VM


object Prerequisites {
  trait NoPruning extends PruningModeComponent {
    val pruningMode = ArchivePruning
  }

  trait Storages extends SharedLevelDBDataSources with NoPruning with Storages.DefaultStorages
}

class Prerequisites(config: Config) {

  private def levelDb(dbPath: String): LevelDBDataSource =
    LevelDBDataSource (
      new LevelDbConfig {
        val verifyChecksums: Boolean = true
        val paranoidChecks: Boolean = true
        val createIfMissing: Boolean = true
        val path: String = dbPath
      }
    )

  private val sourceStorages: Storages = new Storages {
    override lazy val dataSource = levelDb(config.sourceDbPath)
  }

  private val targetStorages: Option[Storages] = config.mode match {
    case DualDB =>
      Some(new Storages {
        override lazy val dataSource = levelDb(config.targetDbPath)
      })

    case SingleDB => None
  }

  val sourceBlockchain = BlockchainImpl(sourceStorages.storages)
  val targetBlockchain = targetStorages.map(ts => BlockchainImpl(ts.storages))

  private val components = new ValidatorsBuilder with BlockchainConfigBuilder with SyncConfigBuilder
    // FIXME What are the semantics after PoW decoupling?
    with ConsensusBuilder with ConsensusConfigBuilder with ShutdownHookBuilder with Logger


  val ledger: Ledger = targetBlockchain match {
    case Some(tb) =>
      new LedgerImpl(VM, tb, components.blockchainConfig, components.syncConfig, components.validators)

    case None =>
      new LedgerImpl(VM, sourceBlockchain, components.blockchainConfig, components.syncConfig, components.validators)
  }

  targetBlockchain.foreach { blockchain =>
    val genesisLoader = new GenesisDataLoader(
      blockchain,
      components.blockchainConfig
    )

    genesisLoader.loadGenesisData()
  }
}
