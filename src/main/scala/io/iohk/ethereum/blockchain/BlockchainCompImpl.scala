package io.iohk.ethereum.blockchain

import io.iohk.ethereum.db.storage._

trait BlockchainCompImpl extends BlockchainComp {

  override val blockchain: Blockchain

  class BlockchainImpl extends Blockchain {
    override protected val blockHeadersStorage: BlockHeadersStorage = storagesComp.storages.blockHeadersStorage
    override protected val blockBodiesStorage: BlockBodiesStorage = storagesComp.storages.blockBodiesStorage
    override protected val blockNumberMappingStorage: BlockNumberMappingStorage = storagesComp.storages.blockNumberMappingStorage
    override protected val evmCodeStorage: EvmCodeStorage = storagesComp.storages.evmCodeStorage
    override protected val receiptStorage: ReceiptStorage = storagesComp.storages.receiptStorage
  }
}
