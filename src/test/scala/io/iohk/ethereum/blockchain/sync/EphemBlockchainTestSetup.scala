package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{EphemDataSourceComponent, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.ledger.Ledger.VMImpl
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
}
