package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit

import io.iohk.ethereum.utils.Config.NodeCacheConfig
import com.google.common.cache
import com.google.common.cache.{CacheBuilder, RemovalNotification}

import scala.collection.JavaConverters.asScalaIterator
import scala.concurrent.duration.FiniteDuration

class LruCache[K <: AnyRef, V <: AnyRef](
    config: NodeCacheConfig,
    notificationHandler: Option[RemovalNotification[K, V] => Unit] = None
) extends Cache[K, V] {

  @volatile private[this] var lastClear = System.nanoTime()

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

  override def drain(): Seq[(K, V)] = {
    this.lastClear = System.nanoTime()
    val mapView = lruCache.asMap
    val iter = mapView.entrySet.iterator
    val result = asScalaIterator(iter).map { e =>
      val key = e.getKey
      val value = mapView.remove(key)
      key -> value
    }
    // `toSeq` returns a lazy Stream!
    // That does not fit here.
    result.toList
  }

  override def getValues: Seq[(K, V)] = {
    import scala.collection.JavaConverters._
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
    FiniteDuration(System.nanoTime() - lastClear, TimeUnit.NANOSECONDS) >= config.maxHoldTime
  }

}
