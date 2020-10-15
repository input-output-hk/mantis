package io.iohk.ethereum.db.storage

import java.net.URI

import io.iohk.ethereum.db.dataSource.{DataSource, DataSourceBatchUpdate}

/**
  * This class is used to store discovered nodes
  *   Value: stored nodes list
  */
class KnownNodesStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[String, Set[String]]{
  val key = "KnownNodes"

  val namespace: IndexedSeq[Byte] = Namespaces.KnownNodesNamespace
  def keySerializer: String => IndexedSeq[Byte] = _.getBytes
  def keyDeserializer: IndexedSeq[Byte] => String = k => new String(k.toArray)
  def valueSerializer: Set[String] => IndexedSeq[Byte] = _.mkString(" ").getBytes
  def valueDeserializer: IndexedSeq[Byte] => Set[String] = (valueBytes: IndexedSeq[Byte]) => new String(valueBytes.toArray).split(' ').toSet

  def getKnownNodes(): Set[URI] = {
    get(key).getOrElse(Set.empty).filter(_.nonEmpty).map(new URI(_))
  }

  def updateKnownNodes(toAdd: Set[URI] = Set.empty, toRemove: Set[URI] = Set.empty): DataSourceBatchUpdate = {
    val updated = (getKnownNodes() ++ toAdd) -- toRemove
    put(key, updated.map(_.toString))
  }

}
