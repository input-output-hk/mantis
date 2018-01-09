package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder
import io.iohk.ethereum.vm.VM


trait EphemBlockchainTestSetup extends ScenarioSetup {
  sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  //+ cake overrides
  override lazy val vm: VM = VM
  override lazy val storagesInstance = new SharedEphemDataSources with LocalPruningConfigBuilder with Storages.DefaultStorages
  //- cake overrides

  // FIXME Delete. Only the dependency above is needed.
  // override lazy val blockchain: BlockchainImpl = BlockchainImpl(storagesInstance.storages)
}
