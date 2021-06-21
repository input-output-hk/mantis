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
      stateStorage = storages.stateStorage
    ) {
      override def getWorldStateProxy(
          blockNumber: BigInt,
          accountStartNonce: UInt256,
          stateRootHash: ByteString,
          noEmptyAccounts: Boolean,
          ethCompatibleStorage: Boolean
      ): InMemoryWorldStateProxy =
        TestModeWorldStateProxy(
          evmCodeStorage,
          stateStorage.getBackingStorage(blockNumber),
          accountStartNonce,
          (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
          stateRootHash,
          noEmptyAccounts,
          ethCompatibleStorage,
          key => preimages.put(crypto.kec256(key.bytes), key)
        )
    }
  }

}
