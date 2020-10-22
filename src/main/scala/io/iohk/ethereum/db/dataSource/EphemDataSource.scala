package io.iohk.ethereum.db.dataSource

import java.nio.ByteBuffer
import io.iohk.ethereum.db.dataSource.DataSource._

class EphemDataSource(var storage: Map[ByteBuffer, Array[Byte]]) extends DataSource {

  /**
    * key.drop to remove namespace prefix from the key
    * @return key values paris from this storage
    */
  def getAll(namespace: Namespace): Seq[(IndexedSeq[Byte], IndexedSeq[Byte])] = synchronized {
    storage.toSeq.map { case (key, value) => (key.array().drop(namespace.length).toIndexedSeq, value.toIndexedSeq) }
  }

  override def get(namespace: Namespace, key: Key): Option[Value] = {
    storage.get(ByteBuffer.wrap((namespace ++ key).toArray)).map(_.toIndexedSeq)
  }

  override def getOptimized(key: Array[Byte]): Option[Array[Byte]] = storage.get(ByteBuffer.wrap(key))

  override def update(dataSourceUpdates: Seq[DataUpdate]): Unit = synchronized {
    dataSourceUpdates.foreach {
      case DataSourceUpdate(namespace, toRemove, toUpsert) =>
        update(namespace, toRemove, toUpsert)
      case DataSourceUpdateOptimized(toRemove, toUpsert) =>
        updateOptimized(toRemove, toUpsert)
    }
  }

  private def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): Unit = synchronized {
    val afterRemoval =
      toRemove.foldLeft(storage)((storage, key) => storage - ByteBuffer.wrap((namespace ++ key).toArray))
    val afterUpdate = toUpsert.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + (ByteBuffer.wrap((namespace ++ toUpdate._1).toArray) -> toUpdate._2.toArray)
    )
    storage = afterUpdate
  }

  private def updateOptimized(toRemove: Seq[Array[Byte]], toUpsert: Seq[(Array[Byte], Array[Byte])]): Unit =
    synchronized {
      val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteBuffer.wrap(key))
      val afterUpdate =
        toUpsert.foldLeft(afterRemoval)((storage, toUpdate) => storage + (ByteBuffer.wrap(toUpdate._1) -> toUpdate._2))
      storage = afterUpdate
    }

  override def clear(): Unit = synchronized {
    storage = Map()
  }

  override def close(): Unit = ()

  override def destroy(): Unit = ()
}

object EphemDataSource {
  def apply(): EphemDataSource = new EphemDataSource(Map())
}
