package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import io.iohk.ethereum.utils.Config.NodeCacheConfig

import scala.collection.{Seq, mutable}
import scala.concurrent.duration.FiniteDuration


class OldCache[K, V](val cache: mutable.Map[K, V], config: NodeCacheConfig) extends Cache[K, V] {

  private val lastClear = new AtomicLong(System.nanoTime())

  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    this.synchronized {
      toRemove.foreach(key => cache -= key)
      toUpsert.foreach(element => cache += element._1 -> element._2)
    }
    this
  }

  override def getValues: Seq[(K, V)] = {
    cache.toSeq
  }

  override def get(key: K): Option[V] = {
    this.synchronized {
      cache.get(key)
    }
  }

  override def clear(): Unit = {
    lastClear.getAndSet(System.nanoTime())
    cache.clear()
  }

  override def shouldPersist: Boolean = {
    cache.size > config.maxSize || isTimeToClear
  }

  private def isTimeToClear: Boolean = {
    FiniteDuration(System.nanoTime(), TimeUnit.NANOSECONDS) - FiniteDuration(
      lastClear.get(),
      TimeUnit.NANOSECONDS
    ) >= config.maxHoldTime
  }
}

object OldCache {

  def getMap[K, V]: mutable.Map[K, V] = mutable.Map.empty

  def createCache[K, V](config: NodeCacheConfig): OldCache[K, V] = {
    new OldCache[K, V](getMap[K, V], config)
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
