package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit

import io.iohk.ethereum.utils.Config.NodeCacheConfig

import scala.collection.Seq
import scala.collection.concurrent.{TrieMap, Map => CMap}
import scala.concurrent.duration.FiniteDuration


class MapCache[K, V](val cache: CMap[K, V], config: NodeCacheConfig) extends Cache[K, V] {

  @volatile private[this] var lastClear = System.nanoTime()

  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(key => cache -= key)
    toUpsert.foreach(element => cache += element._1 -> element._2)
    this
  }

  override def getValues: Seq[(K, V)] = {
    cache.toSeq
  }

  override def get(key: K): Option[V] = {
    cache.get(key)
  }

  override def drain: Seq[(K, V)] = {
    lastClear = System.nanoTime()
    cache.toList.map { case tuple @ (k, _) =>
      cache -= k
      tuple
    }
  }

  override def shouldPersist: Boolean = {
    cache.size > config.maxSize || isTimeToClear
  }

  private def isTimeToClear: Boolean = {
    FiniteDuration(System.nanoTime() - lastClear, TimeUnit.NANOSECONDS) >= config.maxHoldTime
  }
}

object MapCache {

  def getMap[K, V]: CMap[K, V] = TrieMap.empty

  def createCache[K, V](config: NodeCacheConfig): MapCache[K, V] = {
    new MapCache[K, V](getMap[K, V], config)
  }

  private case class TestCacheConfig(override val maxSize: Long, override val maxHoldTime: FiniteDuration)
      extends NodeCacheConfig

  def createTestCache[K, V](
      maxSize: Long,
      maxHoldTime: FiniteDuration = FiniteDuration(5, TimeUnit.MINUTES)
  ): Cache[K, V] = {
    createCache[K, V](TestCacheConfig(maxSize, maxHoldTime))
  }

}
