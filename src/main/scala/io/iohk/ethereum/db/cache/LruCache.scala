package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import com.google.common.cache
import com.google.common.cache.{CacheBuilder, RemovalNotification}
import scala.concurrent.duration.FiniteDuration

class LruCache[K <: AnyRef, V <: AnyRef](
    config: NodeCacheConfig,
    notificationHandler: Option[RemovalNotification[K, V] => Unit] = None
) extends Cache[K, V] {

  private val lastClear = new AtomicLong(System.nanoTime())

  private val lruCache: cache.Cache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(config.maxSize)
      .removalListener((notification: RemovalNotification[K, V]) =>
        if (notification.wasEvicted()) {
          notificationHandler.foreach(handler => handler(notification))
        }
      )
      .build()

  override def clear(): Unit = {
    lastClear.getAndSet(System.nanoTime())
    lruCache.invalidateAll()
  }

  override def getValues: Seq[(K, V)] = {
    import scala.jdk.CollectionConverters._
    lruCache.asMap().asScala.toSeq
  }

  override def shouldPersist: Boolean = isTimeToClear

  override def get(key: K): Option[V] = Option(lruCache.getIfPresent(key))

  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): Cache[K, V] = {
    toRemove.foreach(key => lruCache.invalidate(key))
    toUpsert.foreach(keyValue => lruCache.put(keyValue._1, keyValue._2))
    this
  }

  private def isTimeToClear: Boolean = {
    FiniteDuration(System.nanoTime(), TimeUnit.NANOSECONDS) - FiniteDuration(
      lastClear.get(),
      TimeUnit.NANOSECONDS
    ) >= config.maxHoldTime
  }
}
