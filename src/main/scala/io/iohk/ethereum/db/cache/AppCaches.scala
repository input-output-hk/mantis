package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.StorageTypes.NodeEncoded
import io.iohk.ethereum.db.storage.StorageTypes.NodeHash
import io.iohk.ethereum.utils.Config

trait AppCaches extends CacheComponent {
  val caches: Caches = new Caches {
    override val nodeCache: Cache[NodeHash, NodeEncoded] = MapCache.createCache(Config.NodeCacheConfig)
  }
}
