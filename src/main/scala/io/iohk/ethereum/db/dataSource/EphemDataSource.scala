package io.iohk.ethereum.db.dataSource

case class EphemDataSource(storage: Map[IndexedSeq[Byte], IndexedSeq[Byte]]) extends DataSource {

  override def get(namespace: Namespace, key: Key): Option[Value] = storage.get(namespace ++: key)

  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - (namespace ++ key))
    val afterUpdate = toUpsert.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + ((namespace ++ toUpdate._1) -> toUpdate._2))
    EphemDataSource(afterUpdate)
  }

  override def clear: DataSource = EphemDataSource(Map())

  override def close(): Unit = ()

  override def destroy(): Unit = ()
}

object EphemDataSource {
  def apply(): EphemDataSource = EphemDataSource(Map())
}
