package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.DataSource

private[storage] trait KeyValueStorage[K, V] {
  type T <: KeyValueStorage[K, V]

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
    * This function updates the KeyValueStorage by deleting, updating and inserting new (key-value) pairs
    * in the current namespace.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStorage.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStorage.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new KeyValueStorage after the removals and insertions were done.
    */
  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): T = {
    val newDataSource = dataSource.update(
      namespace = namespace,
      toRemove = toRemove.map(keySerializer),
      toUpsert = toUpsert.map { case (k, v) => keySerializer(k) -> valueSerializer(v) }
    )
    apply(newDataSource)
  }

  /**
    * This function updates the KeyValueStorage by inserting the(key-value) pairs in the current namespace.
    *
    * @param key
    * @param value
    * @return the new KeyValueStorage after the insertion was done.
    */
  def put(key: K, value: V): T = update(Seq(), Seq(key -> value))

  /**
    * This function updates the KeyValueStorage by deleting the (key-value) associated with the passed key
    * from the current namespace.
    *
    * @param key
    * @return the new KeyValueStorage after the deletion was done.
    */
  def remove(key: K): T = update(Seq(key), Seq())

}

object Namespaces {
  val ReceiptsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)
  val HeaderNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
  val BodyNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('b'.toByte)
  val NodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('n'.toByte)
  val CodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('c'.toByte)
  val TotalDifficultyNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('t'.toByte)
  val AppStateNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('s'.toByte)
  val HeightsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('i'.toByte)
  val NodeHashSetNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
}
