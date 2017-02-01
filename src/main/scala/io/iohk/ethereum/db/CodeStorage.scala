package io.iohk.ethereum.db

object CodeStorage {
  type CodeHash = Array[Byte]
  type CodeStorage = KeyValueStorage[CodeHash, Array[Byte]]

  def apply(dataSource: DataSource): CodeStorage = new CodeStorage(
    dataSource = dataSource,
    keySerializer = identity,
    valueSerializer = identity,
    valueDeserializer = identity
  )
}
