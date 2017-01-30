package io.iohk.ethereum.db

class KeyValueStorage[K, V](val dataSource: DataSource,
                            private val keySerializer: K => Array[Byte],
                            private val valueSerializer: V => Array[Byte],
                            private val valueDeserializer: Array[Byte] => V) {

  def get(key: K): Option[V] = {
    val serializedKey = keySerializer(key)
    val serializedValue = dataSource.get(serializedKey)
    serializedValue.map(valueDeserializer)
  }

  def update(toRemove: Seq[K], toUpdate: Seq[(K, V)]): KeyValueStorage[K, V] =
    new KeyValueStorage[K, V](
      dataSource.update(
        version = Array.emptyByteArray,
        toRemove = toRemove.map(keySerializer),
        toUpdate = toUpdate.map { case (k, v) => keySerializer(k) -> valueSerializer(v) }
      ),
      keySerializer,
      valueSerializer,
      valueDeserializer
    )

  def put(key: K, value: V): KeyValueStorage[K, V] = update(Seq(), Seq(key -> value))

  def remove(key: K): KeyValueStorage[K, V] = update(Seq(key), Seq())
}
