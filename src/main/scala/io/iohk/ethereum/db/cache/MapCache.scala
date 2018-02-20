package io.iohk.ethereum.db.cache

import scala.collection.{Seq, mutable}

class MapCache[K,V](val cache: mutable.Map[K, Option[V]]) extends Cache[K, V] {
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(key => cache += key -> None)
    toUpsert.foreach(element => cache += element._1 -> Some(element._2))
    this
  }

  override def getChanges: (Seq[K], Seq[(K, V)]) = {
    var toRemove = List.empty[K]
    var toUpdate = List.empty[(K, V)]

    cache.foreach {
      case (key, Some(value)) => toUpdate = (key, value) :: toUpdate
      case (key, None)        => toRemove = key :: toRemove
    }

    (toRemove, toUpdate)
  }

  override def get(key: K): Option[V] = cache.get(key).flatten

  override def clear: Unit = {
    cache.clear()
  }
}