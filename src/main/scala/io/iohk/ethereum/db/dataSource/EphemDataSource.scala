package io.iohk.ethereum.db.dataSource

class EphemDataSource(var storage: Map[IndexedSeq[Byte], IndexedSeq[Byte]]) extends DataSource {

  override def get(namespace: Namespace, key: Key): Option[Value] = storage.get(namespace ++: key)

  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - (namespace ++ key))
    val afterUpdate = toUpsert.foldLeft(afterRemoval)((storage, toUpdate) =>
      storage + ((namespace ++ toUpdate._1) -> toUpdate._2))
    storage = afterUpdate
    this
  }

  override def clear: DataSource = {
    storage = Map()
    this
  }

  override def close(): Unit = ()

  override def destroy(): Unit = ()
}

object EphemDataSource {
  def apply(): EphemDataSource = new EphemDataSource(Map())
}
