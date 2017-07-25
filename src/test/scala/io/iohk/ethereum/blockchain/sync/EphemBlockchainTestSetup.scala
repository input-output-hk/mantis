package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.BlockchainImpl


trait EphemBlockchainTestSetup {

  trait Pruning extends PruningModeComponent {
    override val pruningMode: PruningMode = ArchivePruning
  }

  val storagesInstance =  new SharedEphemDataSources with Pruning with Storages.DefaultStorages

  val blockchain = BlockchainImpl(storagesInstance.storages)
}
