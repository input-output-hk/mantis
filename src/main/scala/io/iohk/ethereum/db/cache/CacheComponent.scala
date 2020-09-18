package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.NodeStorage

trait CacheComponent {
  val caches: Caches

  trait Caches {
    val nodeCache: Cache[NodeStorage.NodeHash, NodeStorage.NodeEncoded]
  }
}
