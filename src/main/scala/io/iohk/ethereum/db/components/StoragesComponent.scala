package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.BlockchainStorages

trait StoragesComponent {

  val storages: Storages

  trait Storages extends BlockchainStorages {

    val blockHeadersStorage: BlockHeadersStorage

    val blockBodiesStorage: BlockBodiesStorage

    val blockNumberMappingStorage: BlockNumberMappingStorage

    val receiptStorage: ReceiptStorage

    val mptNodeStorage: MptNodeStorage

    val nodeStorage: NodeStorage //FIXME This storage is similar to MPTNodesStorage but

    val evmCodeStorage: EvmCodeStorage

    val totalDifficultyStorage: TotalDifficultyStorage

    val appStateStorage: AppStateStorage

    val fastSyncStateStorage: FastSyncStateStorage
  }
}
