package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.{Archive, PruningMode}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}


trait EphemBlockchainTestSetup {
  val storagesInstance =  new SharedEphemDataSources with Storages.DefaultStorages {
    override val pruningMode: PruningMode = Archive
  }
  val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)
}
