package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.utils.Config

trait AppCaches extends CacheComponent {
  val caches = new Caches {
    override val nodeCache: Cache[NodeHash, NodeEncoded] = MapCache.createCache(Config.NodeCacheConfig)
  }
}
