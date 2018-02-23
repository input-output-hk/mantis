package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder
import io.iohk.ethereum.vm.VM


trait EphemBlockchainTestSetup extends ScenarioSetup {
  lazy val vm: VM = VM

  sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
    override lazy val pruningMode: PruningMode = ArchivePruning
  }

  override lazy val storagesInstance = new SharedEphemDataSources with LocalPruningConfigBuilder with Storages.DefaultStorages
  override lazy val blockchain: BlockchainImpl = BlockchainImpl(storagesInstance.storages)
}
