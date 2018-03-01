package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import scala.collection.{Seq, mutable}
import scala.concurrent.duration.FiniteDuration

// TODO class is not thread safe, although it should not be a problem as all updates are done from actor
class MapCache[K,V](val cache: mutable.Map[K, V], config: NodeCacheConfig) extends Cache[K, V] {

  var lastClear = System.nanoTime()

  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(key     => cache -= key)
    toUpsert.foreach(element => cache += element._1 -> element._2)
    this
  }

  override def getValues: Seq[(K, V)] = {
    cache.toSeq
  }

  override def get(key: K): Option[V] = cache.get(key)

  override def clear: Unit = {
    lastClear = System.nanoTime()
    cache.clear()
  }

  def shouldPersist: Boolean = {
    if (cache.size > config.maxSize || isTimeToClear)
      true
    else
      false
  }

  private def isTimeToClear: Boolean = {
    FiniteDuration(System.nanoTime(), TimeUnit.NANOSECONDS) - FiniteDuration(lastClear, TimeUnit.NANOSECONDS) >= config.maxHoldTime
  }
}

object MapCache {

  def getMap[K,V]: mutable.Map[K, V] = mutable.Map.empty

  def createCache[K, V](config: NodeCacheConfig): MapCache[K, V] = {
    new MapCache[K,V](getMap[K,V], config)
  }
}
