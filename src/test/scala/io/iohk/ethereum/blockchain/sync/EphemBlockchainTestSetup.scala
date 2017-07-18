package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}


trait EphemBlockchainTestSetup {
  val storagesInstance =  new SharedEphemDataSources with Storages.DefaultStorages {
    override val pruningMode: PruningMode = ArchivePruning
  }
  val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)
}
