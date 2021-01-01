package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.db.dataSource.{DataSource, DataSourceBatchUpdate, DataSourceUpdate}
import monix.reactive.Observable

import scala.collection.immutable.ArraySeq

/**
  * Represents transactional key value storage mapping keys of type K to values of type V
  * Note: all methods methods that perform updates return [[io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate]]
  * meaning no updates are actually saved in the underlying DataSource until `.commit()` is called.
  */
trait TransactionalKeyValueStorage[K, V] {

  val dataSource: DataSource
  val namespace: IndexedSeq[Byte]
  def keySerializer: K => IndexedSeq[Byte]
  def valueSerializer: V => IndexedSeq[Byte]
  def valueDeserializer: IndexedSeq[Byte] => V
  def keyDeserializer: IndexedSeq[Byte] => K

  /**
    * This function obtains the associated value to a key in the current namespace, if there exists one.
    *
    * @param key
    * @return the value associated with the passed key, if there exists one.
    */
  def get(key: K): Option[V] = dataSource.get(namespace, keySerializer(key)).map(valueDeserializer)

  /**
    * This function creates a batch of updates to the KeyValueStorage by deleting, updating and inserting new (key-value)
    * pairs in the current namespace. The batch should be committed atomically.
    */
  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): DataSourceBatchUpdate = {
    DataSourceBatchUpdate(
      dataSource,
      Array(
        DataSourceUpdate(
          namespace,
          toRemove.map(keySerializer),
          toUpsert.map { case (k, v) =>
            keySerializer(k) -> valueSerializer(v)
          }
        )
      )
    )
  }

  def put(key: K, value: V): DataSourceBatchUpdate =
    update(Nil, Seq(key -> value))

  def remove(key: K): DataSourceBatchUpdate =
    update(Seq(key), Nil)

  def emptyBatchUpdate: DataSourceBatchUpdate =
    DataSourceBatchUpdate(dataSource, Array.empty)

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
