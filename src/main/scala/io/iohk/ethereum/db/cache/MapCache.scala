package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantReadWriteLock

import io.iohk.ethereum.utils.Config.NodeCacheConfig

import scala.collection.{Seq, mutable}
import scala.concurrent.duration.FiniteDuration

//TODO EC-492 Investigate more carefully possibility of having read cache in front of db
// class is now entirely thread safe
class MapCache[K, V](val cache: mutable.Map[K, V], config: NodeCacheConfig) extends Cache[K, V] {

  @volatile private[this] var lastClear = System.nanoTime()

  private[this] val lock = new ReentrantReadWriteLock()

  private[this] def lockForReading[R](f: => R): R = {
    lock.readLock().lock()
    try { f } finally lock.readLock().unlock()
  }

  private[this] def lockForWriting[R](f: => R): R = {
    lock.writeLock().lock()
    try { f } finally lock.writeLock().unlock()
  }

  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = lockForWriting {
    toRemove.foreach(key => cache -= key)
    toUpsert.foreach(element => cache += element._1 -> element._2)
    this
  }

  override def getValues: Seq[(K, V)] = lockForReading {
     cache.toSeq
  }

  override def get(key: K): Option[V] = lockForReading {
     cache.get(key)
  }

  override def clear(): Unit = lockForWriting {
    lastClear = System.nanoTime()
    cache.clear()
  }

  override def shouldPersist: Boolean = lockForReading {
    cache.size > config.maxSize || isTimeToClear
  }

  private def isTimeToClear: Boolean = {
    FiniteDuration(System.nanoTime() - lastClear, TimeUnit.NANOSECONDS) >= config.maxHoldTime
  }
}

object MapCache {

  def getMap[K, V]: mutable.Map[K, V] = mutable.Map.empty

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
