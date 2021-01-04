package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.cache.Cache

trait CachedKeyValueStorage[K, V, T <: CachedKeyValueStorage[K, V, T]] extends SimpleMap[K, V, T] {
  type I <: KeyValueStorage[K, V, I]
  protected val storage: I
  protected val cache: Cache[K, V]
  def apply(cache: Cache[K, V], storage: I): T

  def get(key: K): Option[V] = cache.get(key) orElse storage.get(key)

  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): T = {
    cache.update(toRemove, toUpsert)
    apply(cache, storage)
  }

  def updateCond(toRemove: Seq[K], toUpsert: Seq[(K, V)], inMemory: Boolean): T = {
    if (inMemory)
      cache.update(toRemove, toUpsert)
    else
      storage.update(toRemove, toUpsert)

    apply(cache, storage)
  }

  def forcePersist(): Unit = {
    storage.update(Nil, cache.getValues)
    cache.clear()
  }

  // TODO EC-491 Consider other persist strategy like sliding window (save and clear only old stuff which survived long enough)
  def persist(): Boolean = {
    if (cache.shouldPersist) {
      forcePersist()
      true
    } else {
      false
    }
  }
}
