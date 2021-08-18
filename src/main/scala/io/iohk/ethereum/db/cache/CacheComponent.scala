package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.StorageTypes.NodeEncoded
import io.iohk.ethereum.db.storage.StorageTypes.NodeHash

trait CacheComponent {
  val caches: Caches

  trait Caches {
    val nodeCache: Cache[NodeHash, NodeEncoded]
  }
}
