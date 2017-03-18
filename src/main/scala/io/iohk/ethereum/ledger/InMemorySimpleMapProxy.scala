package io.iohk.ethereum.ledger

import io.iohk.ethereum.common.SimpleMap

object InMemorySimpleMapProxy {
  def wrap[K, V, I <: SimpleMap[K, V]](inner: WrappedMap[K, V, I]): InMemorySimpleMapProxy[K, V, I] =
    new InMemorySimpleMapProxy(inner, Map.empty[K, Option[V]])
}

trait WrappedMap[K, V, I <: SimpleMap[K, V]] extends SimpleMap[K, V] {
  override type T = WrappedMap[K, V, I]
  val wrapped: I
}

/**
  * This class keeps holds changes made to the inner [[SimpleMap]] until data is commited
  *
  * @param inner [[SimpleMap]] to proxy
  * @param cache InMemory map where data is going to be cached
  * @tparam K data type of the key to be used within this Proxy
  * @tparam V data type of the value to be used within this Proxy
  */
class InMemorySimpleMapProxy[K, V, I <: SimpleMap[K, V]] private(val inner: WrappedMap[K, V, I], private val cache: Map[K, Option[V]])
  extends SimpleMap[K, V] {

  override type T = InMemorySimpleMapProxy[K, V, I]

  type Changes = (Seq[K], Seq[(K, V)])

  def changes: Changes = cache.foldLeft(Seq.empty[K] -> Seq.empty[(K, V)]) { (acc, cachedItem) =>
    cachedItem match {
      case (key, Some(value)) => (acc._1, acc._2 :+ key -> value)
      case (key, None) => (acc._1 :+ key, acc._2)
    }
  }

  /**
    * Persists the changes into the underlying [[SimpleMap]]
    *
    * @return Updated proxy
    */
  def persist(): InMemorySimpleMapProxy[K, V, I] = {
    val changesToApply = changes
    new InMemorySimpleMapProxy[K, V, I](inner.update(changesToApply._1, changesToApply._2), Map())
  }

  /**
    * Clears the cache without applying the changes
    *
    * @return Updated proxy
    */
  def rollback: InMemorySimpleMapProxy[K, V, I] = new InMemorySimpleMapProxy[K, V, I](inner, Map())

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  def get(key: K): Option[V] = cache.getOrElse(key, inner.get(key)) //FIXME We can cache retrieved values too

  def wrapped: I = inner.wrapped

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): InMemorySimpleMapProxy[K, V, I] = {
    val afterRemoval = toRemove.foldLeft(cache) { (updated, key) => updated + (key -> None) }
    val afterInserts = toUpsert.foldLeft(afterRemoval) { (updated, toUpsert) => updated + (toUpsert._1 -> Some(toUpsert._2)) }
    new InMemorySimpleMapProxy[K, V, I](inner, afterInserts)
  }
}
