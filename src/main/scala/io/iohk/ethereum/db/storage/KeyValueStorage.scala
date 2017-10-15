package io.iohk.ethereum.db.storage

import io.iohk.ethereum.common.{BatchOperation, Removal, SimpleMap, Upsert}
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Value}

trait KeyValueStorage[K, V, T <: KeyValueStorage[K, V, T]] extends SimpleMap[K, V, T]{

  val dataSource: DataSource
  val namespace: IndexedSeq[Byte]
  def keySerializer: K => IndexedSeq[Byte]
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
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param batchOperations sequence of operations to be applied
    * @return the new DataSource after the removals and insertions were done.
    */
  def update(batchOperations: Seq[BatchOperation[K, V]]): T = {
    val newDataSource = dataSource.update(
      namespace = namespace,
      batchOperations.map {
        case Upsert(k, v) => Upsert[Key, Value](keySerializer(k), valueSerializer(v))
        case Removal(k) => Removal[Key, Value](keySerializer(k))
      }
    )
    apply(newDataSource)
  }
}

object Namespaces {
  val ReceiptsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)
  val HeaderNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
  val BodyNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('b'.toByte)
  val NodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('n'.toByte)
  val CodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('c'.toByte)
  val TotalDifficultyNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('t'.toByte)
  val AppStateNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('s'.toByte)
  val KnownNodesNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('k'.toByte)
  val HeightsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('i'.toByte)
  val FastSyncStateNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
  val TransactionMappingNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('l'.toByte)
}
