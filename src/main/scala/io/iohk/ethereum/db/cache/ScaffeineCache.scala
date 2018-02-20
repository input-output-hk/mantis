package io.iohk.ethereum.db.cache

import com.github.blemale.scaffeine

import scala.collection.Seq

class ScaffeineCache[K,V](val cache: scaffeine.Cache[K, Option[V]]) extends Cache[K, V] {
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(k => cache.put(k, None))
    toUpsert.foreach(elem => cache.put(elem._1, Some(elem._2)))
    this
  }

  override def getChanges: (Seq[K], Seq[(K, V)]) = {
    var toRemove = List.empty[K]
    var toUpdate = List.empty[(K, V)]

    cache.asMap().foreach {
      case (key, Some(value)) => toUpdate = (key, value) :: toUpdate
      case (key, None)        => toRemove = key :: toRemove

    }
    (toRemove, toUpdate)
  }

  override def get(key: K): Option[V] = cache.getIfPresent(key).flatten

  override def clear: Unit = cache.invalidateAll()
}
