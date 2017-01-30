package io.iohk.ethereum.db

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class IodbDataSource(lSMStore: LSMStore) extends DataSource {

  import IodbDataSource._

  override def get(key: Array[Byte]): Option[Array[Byte]] = lSMStore.get(ByteArrayWrapper(key)).map(v => v.data)

  override def update(version: Array[Byte], toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource = {
    lSMStore.update(ByteArrayWrapper(storageVersionGen(version)), toRemove.map(key => ByteArrayWrapper(key)), asStorables(toUpdate))
    new IodbDataSource(lSMStore)
  }

  private def asStorables(keyValues: Seq[(Array[Byte], Array[Byte])]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
    keyValues.map(kv => ByteArrayWrapper(kv._1) -> ByteArrayWrapper(kv._2))
}


object IodbDataSource {
  private val updateCounter = new AtomicLong(System.currentTimeMillis())

  private def storageVersionGen(rootHash: Array[Byte]): Array[Byte] = {
    rootHash ++ ByteBuffer.allocate(java.lang.Long.SIZE / java.lang.Byte.SIZE).putLong(updateCounter.incrementAndGet()).array()
  }
}
