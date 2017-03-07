package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.BlockchainStorages

trait StoragesComponent {

  def storages: Storages

  trait Storages extends BlockchainStorages {

    def blockHeadersStorage: BlockHeadersStorage

    def blockBodiesStorage: BlockBodiesStorage

    def blockNumberMappingStorage: BlockNumberMappingStorage

    def receiptStorage: ReceiptStorage

    def mptNodeStorage: MptNodeStorage

    def evmCodeStorage: EvmCodeStorage

    def totalDifficultyStorage: TotalDifficultyStorage

    def appStateStorage: AppStateStorage
  }
}
