package io.iohk.ethereum.blockchain

import io.iohk.ethereum.db.storage._

trait BlockchainCompImpl extends BlockchainComp {

  override val blockchain: Blockchain = new BlockchainImpl

  class BlockchainImpl extends Blockchain {
    override def blockHeadersStorage: BlockHeadersStorage = storagesComp.storages.blockHeadersStorage
    override def blockBodiesStorage: BlockBodiesStorage = storagesComp.storages.blockBodiesStorage
    override def blockNumberMappingStorage: BlockNumberMappingStorage = storagesComp.storages.blockNumberMappingStorage
    override def evmCodeStorage: EvmCodeStorage = storagesComp.storages.evmCodeStorage
    override def receiptStorage: ReceiptStorage = storagesComp.storages.receiptStorage
  }
}
