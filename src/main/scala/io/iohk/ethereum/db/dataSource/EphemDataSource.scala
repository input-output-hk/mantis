package io.iohk.ethereum.db.dataSource

import akka.util.ByteString

case class EphemDataSource(storage: Map[ByteString, Array[Byte]]) extends DataSource {

  override def get(namespace: Byte, key: Array[Byte]): Option[Array[Byte]] = storage.get(ByteString(namespace +: key))

  override def update(namespace: Byte, toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteString(namespace +: key))
    val afterUpdate = toUpdate.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + (ByteString(namespace +: toUpdate._1) -> toUpdate._2))
    EphemDataSource(afterUpdate)
  }
}

object EphemDataSource {
  def apply(): EphemDataSource = EphemDataSource(Map())
}
