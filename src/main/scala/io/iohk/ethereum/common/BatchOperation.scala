package io.iohk.ethereum.common

sealed trait BatchOperation[K, V]
case class Upsert[K, V](key: K, value: V) extends BatchOperation[K, V]
case class Removal[K, V](key: K) extends BatchOperation[K, V]

object Upsert {
  def apply[K, V](t: (K, V)): Upsert[K,V] = Upsert[K, V](t._1, t._2)
}
