package io.iohk.ethereum.db.cache

import io.iohk.ethereum.common.SimpleMap
import scala.collection.Seq

//TODO interface is stil pretty minimial, explore other methods
trait Cache[K, V] extends SimpleMap[K, V, Cache[K,V]] {
  def size: Long
  def getChanges: (Seq[K], Seq[(K, V)])
  def clear: Unit
}
