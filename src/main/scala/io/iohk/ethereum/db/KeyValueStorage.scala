package io.iohk.ethereum.db

trait KeyValueStorage[K, V] {
  type T <: KeyValueStorage[K, V]

  val dataSource: DataSource
  def keySerializer: K => Array[Byte]
  def valueSerializer: V => Array[Byte]
  def valueDeserializer: Array[Byte] => V

  def apply(dataSource: DataSource): T

  def get(key: K): Option[V] = {
    val serializedKey = keySerializer(key)
    val serializedValue = dataSource.get(serializedKey)
    serializedValue.map(valueDeserializer)
  }

  def update(toRemove: Seq[K], toUpdate: Seq[(K, V)]): T = {
    val newDataSource = dataSource.update(
      toRemove = toRemove.map(keySerializer),
      toUpdate = toUpdate.map { case (k, v) => keySerializer(k) -> valueSerializer(v) }
    )
    apply(newDataSource)
  }

  def put(key: K, value: V): T = update(Seq(), Seq(key -> value))

  def remove(key: K): T = update(Seq(key), Seq())

}
