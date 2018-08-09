package io.iohk.ethereum.db.dataSource

import java.nio.ByteBuffer

class EphemDataSource(var storage: Map[ByteBuffer, Array[Byte]]) extends DataSource {

  /**
    * key.drop to remove namespace prefix from the key
    * @return key values paris from this storage
    */
  def getAll(namespace: Namespace): Seq[(IndexedSeq[Byte], IndexedSeq[Byte])] =
    storage.toSeq.map{case (key, value) => (key.array().drop(namespace.length).toIndexedSeq, value.toIndexedSeq)}

  override def get(namespace: Namespace, key: Key): Option[Value] = storage.get(ByteBuffer.wrap((namespace ++ key).toArray)).map(_.toIndexedSeq)

  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteBuffer.wrap((namespace ++ key).toArray))
    val afterUpdate = toUpsert.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + (ByteBuffer.wrap((namespace ++ toUpdate._1).toArray) -> toUpdate._2.toArray))
    storage = afterUpdate
    this
  }

  override def clear: DataSource = {
    storage = Map()
    this
  }

  override def close(): Unit = ()

  override def destroy(): Unit = ()

  override def updateOptimized(toRemove: Seq[Array[Byte]], toUpsert: Seq[(Array[Byte], Array[Byte])]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteBuffer.wrap(key))
    val afterUpdate = toUpsert.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + (ByteBuffer.wrap(toUpdate._1) -> toUpdate._2))
    storage = afterUpdate
    this

  }

  override def getOptimized(key: Array[Byte]): Option[Array[Byte]] = storage.get(ByteBuffer.wrap(key))
}

object EphemDataSource {
  def apply(): EphemDataSource = new EphemDataSource(Map())
}
