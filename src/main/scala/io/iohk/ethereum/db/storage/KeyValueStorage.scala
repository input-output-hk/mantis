package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.db.dataSource.{DataSource, DataSourceUpdate}
import monix.reactive.Observable

import scala.collection.immutable.ArraySeq

trait KeyValueStorage[K, V, T <: KeyValueStorage[K, V, T]] extends SimpleMap[K, V, T] {

  val dataSource: DataSource
  val namespace: IndexedSeq[Byte]
  def keySerializer: K => IndexedSeq[Byte]
  def keyDeserializer: IndexedSeq[Byte] => K
  def valueSerializer: V => IndexedSeq[Byte]
  def valueDeserializer: IndexedSeq[Byte] => V

  protected def apply(dataSource: DataSource): T

  /**
    * This function obtains the associated value to a key in the current namespace, if there exists one.
    *
    * @param key
    * @return the value associated with the passed key, if there exists one.
    */
  def get(key: K): Option[V] = dataSource.get(namespace, keySerializer(key)).map(valueDeserializer)

  /**
    * This function updates the KeyValueStorage by deleting, updating and inserting new (key-value) pairs
    * in the current namespace.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStorage.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStorage.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new KeyValueStorage after the removals and insertions were done.
    */
  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): T = {
    dataSource.update(
      Seq(
        DataSourceUpdate(
          namespace,
          toRemove.map(keySerializer),
          toUpsert.map { case (k, v) => keySerializer(k) -> valueSerializer(v) }
        )
      )
    )
    apply(dataSource)
  }

  def storageContent: Observable[Either[IterationError, (K, V)]] = {
    dataSource.iterate(namespace).map { result =>
      result.map { case (key, value) =>
        val kseq = keyDeserializer(ArraySeq.unsafeWrapArray(key))
        val vseq = valueDeserializer(ArraySeq.unsafeWrapArray(value))
        (kseq, vseq)
      }
    }
  }
}
