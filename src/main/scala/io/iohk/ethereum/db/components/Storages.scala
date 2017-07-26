package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.mpt.NodesKeyValueStorage

object Storages {

  trait PruningModeComponent {
    val pruningMode: PruningMode
  }

  trait DefaultStorages extends StoragesComponent {

    dataSourcesComp: DataSourcesComponent with PruningModeComponent =>

    override val storages: Storages = new DefaultStorages(pruningMode)

    class DefaultStorages(override val pruningMode: PruningMode) extends Storages {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override val nodeStorage: NodeStorage = new NodeStorage(dataSources.mptDataSource)

      override val fastSyncStateStorage: FastSyncStateStorage = new FastSyncStateStorage(dataSources.fastSyncStateDataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeDataSource)

      override val totalDifficultyStorage: TotalDifficultyStorage =
        new TotalDifficultyStorage(dataSources.totalDifficultyDataSource)

      override val appStateStorage: AppStateStorage = new AppStateStorage(
        dataSources.appStateDataSource,
        PruningMode.nodesKeyValueStorage(pruningMode, nodeStorage)(None).prune
      )

      override val transactionMappingStorage: TransactionMappingStorage = new TransactionMappingStorage(dataSources.transactionMappingDataSource)

      override val nodesKeyValueStorageFor: (Option[BigInt]) => NodesKeyValueStorage = bn => PruningMode.nodesKeyValueStorage(pruningMode, nodeStorage)(bn)

      override val knownNodesStorage: KnownNodesStorage = new KnownNodesStorage(dataSources.knownNodesDataSource)

    }

  }

  /**
    * As IODB required same length keys, we need a specific storage that pads integer values to be used as keys to match
    * keccak keys. See [[io.iohk.ethereum.db.storage.IodbBlockNumberMappingStorage]]
    */
  trait IodbStorages extends StoragesComponent {
    dataSourcesComp: DataSourcesComponent with PruningModeComponent =>

    override val storages = new DefaultBlockchainStorages(pruningMode)

    class DefaultBlockchainStorages(override val pruningMode: PruningMode) extends Storages {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new IodbBlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override val nodeStorage: NodeStorage = new NodeStorage(dataSources.mptDataSource)

      override val fastSyncStateStorage: FastSyncStateStorage = new FastSyncStateStorage(dataSources.fastSyncStateDataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeDataSource)

      override val totalDifficultyStorage: TotalDifficultyStorage =
        new TotalDifficultyStorage(dataSources.totalDifficultyDataSource)

      override val appStateStorage: AppStateStorage = new AppStateStorage(
        dataSources.appStateDataSource,
        PruningMode.nodesKeyValueStorage(pruningMode, nodeStorage)(None).prune
      )

      override val transactionMappingStorage: TransactionMappingStorage = new TransactionMappingStorage(dataSources.transactionMappingDataSource)

      override val nodesKeyValueStorageFor: (Option[BigInt]) => NodesKeyValueStorage =
        (bn: Option[BigInt]) => PruningMode.nodesKeyValueStorage(pruningMode, nodeStorage)(bn)

      override val knownNodesStorage: KnownNodesStorage = new KnownNodesStorage(dataSources.knownNodesDataSource)
    }
  }
}
