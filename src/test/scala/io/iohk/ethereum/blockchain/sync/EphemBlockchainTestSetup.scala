package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.EphemDataSourceComponent
import io.iohk.ethereum.db.components.Storages
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.BlockchainMetadata
import io.iohk.ethereum.domain.appstate.BestBlockInfo
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder

trait EphemBlockchainTestSetup extends ScenarioSetup {

  trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  //+ cake overrides
  override lazy val vm: VMImpl = new VMImpl
  override lazy val storagesInstance
      : EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages =
    new EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages
  //- cake overrides

  def getNewStorages: EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages =
    new EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages

  def getNewBlockchainMetadata = new BlockchainMetadata(BestBlockInfo(Fixtures.Blocks.Genesis.header.hash, 0), 0)
}
