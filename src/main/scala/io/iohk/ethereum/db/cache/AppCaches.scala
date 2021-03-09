package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.utils.Config

trait AppCaches extends CacheComponent {
  val caches = new Caches {
    override val nodeCache: Cache[NodeHash, NodeEncoded] = MapCache.createCache(Config.NodeCacheConfig)
  }
}
