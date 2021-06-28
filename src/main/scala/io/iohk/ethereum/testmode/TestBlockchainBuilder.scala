package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.nodebuilder.{BlockchainBuilder, StorageBuilder}

trait TestBlockchainBuilder extends BlockchainBuilder {
  self: StorageBuilder =>

  lazy val preimages: collection.concurrent.Map[ByteString, UInt256] =
    new collection.concurrent.TrieMap[ByteString, UInt256]()

  override lazy val blockchain: BlockchainImpl = {
    val storages = storagesInstance.storages
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      evmCodeStorage = storages.evmCodeStorage,
      chainWeightStorage = storages.chainWeightStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage,
      stateStorage = storages.stateStorage,
      blockchainReader = blockchainReader
    )
  }

}
