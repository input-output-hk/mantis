package io.iohk.ethereum.blockchain

import io.iohk.ethereum.db.storage._

trait BlockchainCompImpl extends BlockchainComp {

  override val blockchain: Blockchain = new BlockchainImpl

  class BlockchainImpl extends Blockchain {
    override protected def blockHeadersStorage: BlockHeadersStorage = storagesComp.storages.blockHeadersStorage
    override protected def blockBodiesStorage: BlockBodiesStorage = storagesComp.storages.blockBodiesStorage
    override protected def blockNumberMappingStorage: BlockNumberMappingStorage = storagesComp.storages.blockNumberMappingStorage
    override protected def evmCodeStorage: EvmCodeStorage = storagesComp.storages.evmCodeStorage
    override protected def receiptStorage: ReceiptStorage = storagesComp.storages.receiptStorage
  }
}
