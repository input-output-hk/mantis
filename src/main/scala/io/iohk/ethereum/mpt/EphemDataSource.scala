package io.iohk.ethereum.mpt

import akka.util.ByteString

case class EphemDataSource(storage: Map[ByteString, Array[Byte]]) extends DataSource {

  override def get(key: Array[Byte]): Option[Array[Byte]] = storage.get(ByteString(key))

  override def update(rootHash: Array[Byte], toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteString(key))
    val afterUpdate = toUpdate.foldLeft(afterRemoval)((storage, toUpdate) => storage + (ByteString(toUpdate._1) -> toUpdate._2))
    EphemDataSource(afterUpdate)
  }
}

object EphemDataSource {
  def apply(): EphemDataSource = EphemDataSource(Map())
}