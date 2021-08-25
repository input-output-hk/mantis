package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.BlockchainStorages

trait StoragesComponent {

  val storages: Storages

  trait Storages extends BlockchainStorages {

    val blockMetadataStorage: BlockMetadataStorage

    val blockHeadersStorage: BlockHeadersStorage

    val blockBodiesStorage: BlockBodiesStorage

    val blockNumberMappingStorage: BlockNumberMappingStorage

    val receiptStorage: ReceiptStorage

    val nodeStorage: NodeStorage

    val evmCodeStorage: EvmCodeStorage

    val chainWeightStorage: ChainWeightStorage

    val appStateStorage: AppStateStorage

    val fastSyncStateStorage: FastSyncStateStorage

    val transactionMappingStorage: TransactionMappingStorage

    val knownNodesStorage: KnownNodesStorage

    val pruningMode: PruningMode

  }
}
