package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder


trait EphemBlockchainTestSetup extends ScenarioSetup {
  sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  //+ cake overrides
  override lazy val vm: VMImpl = new VMImpl
  override lazy val storagesInstance = new SharedEphemDataSources with LocalPruningConfigBuilder with Storages.DefaultStorages
  //- cake overrides
}
