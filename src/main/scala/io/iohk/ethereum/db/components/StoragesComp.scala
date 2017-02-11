package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._

trait StoragesComp {

  def storages: Storages

  trait Storages {

    def blockHeadersStorage: BlockHeadersStorage

    def blockBodiesStorage: BlockBodiesStorage

    def blockNumberMappingStorage: BlockNumberMappingStorage

    def receiptStorage: ReceiptStorage

    def mptNodeStorage: MptNodeStorage

    def evmCodeStorage: EvmCodeStorage

  }

}
