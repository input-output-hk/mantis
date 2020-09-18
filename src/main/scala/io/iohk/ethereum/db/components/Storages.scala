package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.cache.{AppCaches, LruCache}
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.utils.Config

object Storages {

  trait PruningModeComponent {
    val pruningMode: PruningMode
  }

  trait DefaultStorages extends StoragesComponent {

    dataSourcesComp: DataSourcesComponent with PruningModeComponent =>

    override val storages: Storages = new DefaultStorages(pruningMode)

    class DefaultStorages(override val pruningMode: PruningMode) extends Storages with AppCaches {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override val nodeStorage: NodeStorage = new NodeStorage(dataSources.mptDataSource)

      override val cachedNodeStorage: CachedNodeStorage = new CachedNodeStorage(nodeStorage, caches.nodeCache)

      override val fastSyncStateStorage: FastSyncStateStorage = new FastSyncStateStorage(dataSources.fastSyncStateDataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeDataSource)

      override val totalDifficultyStorage: TotalDifficultyStorage =
        new TotalDifficultyStorage(dataSources.totalDifficultyDataSource)

      override val appStateStorage: AppStateStorage = new AppStateStorage(dataSources.appStateDataSource)

      override val transactionMappingStorage: TransactionMappingStorage = new TransactionMappingStorage(dataSources.transactionMappingDataSource)

      override val knownNodesStorage: KnownNodesStorage = new KnownNodesStorage(dataSources.knownNodesDataSource)

      override val stateStorage: StateStorage =
        StateStorage(
          pruningMode,
          nodeStorage,
          cachedNodeStorage,
          new LruCache[NodeHash, HeapEntry](Config.InMemoryPruningNodeCacheConfig, Some(CachedReferenceCountedStorage.saveOnlyNotificationHandler(nodeStorage)))
        )

    }
  }
}
