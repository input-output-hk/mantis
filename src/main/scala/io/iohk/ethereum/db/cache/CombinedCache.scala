package io.iohk.ethereum.db.cache

import com.github.blemale.scaffeine

import scala.collection.{Seq, mutable}

class CombinedCache[K,V](val changesCache: mutable.Map[K, Option[V]] , val readCache: scaffeine.Cache[K, V]) extends Cache[K, V] {
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(k => changesCache.put(k, None))
    toUpsert.foreach(elem => changesCache.put(elem._1, Some(elem._2)))
    this
  }

  override def getChanges: (Seq[K], Seq[(K, V)]) = {
    var toRemove = List.empty[K]
    var toUpdate = List.empty[(K, V)]

    changesCache.foreach {
      case (key, Some(value)) =>
        readCache.put(key, value)
        toUpdate = (key, value) :: toUpdate
      case (key, None) =>
        readCache.invalidate(key)
        toRemove = key :: toRemove

    }
    (toRemove, toUpdate)
  }

  override def get(key: K): Option[V] = changesCache.get(key).flatten orElse readCache.getIfPresent(key)

  override def clear: Unit = {
    changesCache.clear()
  }
}