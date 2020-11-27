package io.iohk.ethereum.db.cache

import io.iohk.ethereum.common.SimpleMap
import scala.collection.Seq


trait Cache[K, V] extends SimpleMap[K, V, Cache[K, V]] {
  def getValues: Seq[(K, V)]

  /**
    * Entries are removed from the concurrent map one by one
    *
    * Cache is not guaranteed to be empty after the operation
    * As it can be updated from the other threads
    *
    * @return all the values that were removed from the cache
    */
  def drain: Seq[(K, V)]


  def shouldPersist: Boolean
}
