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

    dataSourcesComp: DataSourceComponent with PruningModeComponent =>

    override val storages: Storages = new DefaultStorages(pruningMode)

    class DefaultStorages(override val pruningMode: PruningMode) extends Storages with AppCaches {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSource)

      override val nodeStorage: NodeStorage = new NodeStorage(dataSource)

      override val cachedNodeStorage: CachedNodeStorage = new CachedNodeStorage(nodeStorage, caches.nodeCache)

      override val fastSyncStateStorage: FastSyncStateStorage = new FastSyncStateStorage(dataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSource)

      override val chainWeightStorage: ChainWeightStorage =
        new ChainWeightStorage(dataSource)

      override val appStateStorage: AppStateStorage = new AppStateStorage(dataSource)

      override val transactionMappingStorage: TransactionMappingStorage = new TransactionMappingStorage(dataSource)

      override val knownNodesStorage: KnownNodesStorage = new KnownNodesStorage(dataSource)

      override val stateStorage: StateStorage =
        StateStorage(
          pruningMode,
          nodeStorage,
          cachedNodeStorage,
          new LruCache[NodeHash, HeapEntry](
            Config.InMemoryPruningNodeCacheConfig,
            Some(CachedReferenceCountedStorage.saveOnlyNotificationHandler(nodeStorage))
          )
        )

    }
  }
}
