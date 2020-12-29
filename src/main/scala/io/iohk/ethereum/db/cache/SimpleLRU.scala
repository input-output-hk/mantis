package io.iohk.ethereum.db.cache

import java.util

/**
  * Simplest-possible implementation of insert-time based LRU
  *
  * @param maxElements - capacity of the inner map, between 0 and this value.
  * @param ttlMillis   - entry invalidation interval
  * @tparam K - stands for the type of the keys
  */
class SimpleLRU[K](maxElements: Int, ttlMillis: Long) {

  private[this] val inner = new util.LinkedHashMap[K, Long](maxElements, 0.75f, false) {
    override def removeEldestEntry(old: util.Map.Entry[K, Long]): Boolean = {
      size() > maxElements || tooOld(old.getValue)
    }
  }

  /**
    * @param key - will be searching and added
    * @return
    *   true  - if there was such entry already
    *   false - if not
    */
  def checkAndRefreshEntry(key: K): Boolean = inner.synchronized {
    val existing = Option(inner.put(key, getCurrentTime))
    existing.exists(!tooOld(_))
  }

  // Override this to test
  protected def getCurrentTime: Long = System.currentTimeMillis()

  private[this] def tooOld(time: Long) = time + ttlMillis < getCurrentTime

}
