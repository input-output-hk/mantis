package io.iohk.ethereum.testmode

import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.BlockchainBuilder
import io.iohk.ethereum.nodebuilder.StorageBuilder

trait TestBlockchainBuilder extends BlockchainBuilder {
  self: StorageBuilder =>

  override lazy val blockchain: BlockchainImpl = {
    val storages = storagesInstance.storages
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      chainWeightStorage = storages.chainWeightStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage,
      stateStorage = storages.stateStorage,
      blockchainReader = blockchainReader,
      blockchainMetadata = blockchainMetadata
    )
  }

}
