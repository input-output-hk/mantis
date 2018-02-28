package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.cache.Cache
import scala.collection._


trait CachedKeyValueStorage[K, V, T <: CachedKeyValueStorage[K, V, T]] extends SimpleMap[K, V, T]{
  type I <: KeyValueStorage[K, V, I]
  val storage: I
  val cache: Cache[K, V]
  def apply(cache: Cache[K,V] , storage: I): T

  val maxSize = 40000

  def get(key: K): Option[V] = cache.get(key) orElse storage.get(key)

  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): T =  {
    cache.update(toRemove, toUpsert)
    apply(cache, storage)
  }

  def persist(): T = {
    val changes = cache.getChanges
    storage.update(changes._1, changes._2)
    apply(cache, storage)
  }

  def clearCache(): T = {
    cache.clear
    apply(cache, storage)
  }
}

