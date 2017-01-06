package io.iohk.ethereum.merklePatriciaTree

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class IodbDataSource(lSMStore: LSMStore) extends DataSource {

  override def get(key: Array[Byte]): Option[Array[Byte]] = lSMStore.get(ByteArrayWrapper(key)).map(v => v.data)

  override def update(key: Array[Byte], value: Array[Byte]): DataSource = {
    lSMStore.update(lSMStore.lastVersion + 1, Seq(), Seq(ByteArrayWrapper(key) -> ByteArrayWrapper(value)))
    new IodbDataSource(lSMStore)
  }

  override def update(toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource = {
    lSMStore.update(lSMStore.lastVersion + 1, toRemove.map(key => ByteArrayWrapper(key)), asStorables(toUpdate))
    new IodbDataSource(lSMStore)
  }

  private def asStorables(keyValues: Seq[(Array[Byte], Array[Byte])]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
    keyValues.map(kv => ByteArrayWrapper(kv._1) -> ByteArrayWrapper(kv._2))

}
