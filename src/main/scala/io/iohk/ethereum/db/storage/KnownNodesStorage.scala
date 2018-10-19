package io.iohk.ethereum.db.storage

import java.net.URI

import io.iohk.ethereum.db.dataSource.DataSource

/**
  * This class is used to store discovered nodes
  *   Value: stored nodes list
  */
class KnownNodesStorage(val dataSource: DataSource) extends KeyValueStorage[String, Set[String], KnownNodesStorage]{
  type T = KnownNodesStorage

  val key = "KnownNodes"

  val namespace: IndexedSeq[Byte] = Namespaces.KnownNodesNamespace
  def keySerializer: String => Array[Byte] = _.getBytes
  def valueSerializer: Set[String] => Array[Byte] = _.mkString(" ").getBytes
  def valueDeserializer: Array[Byte] => Set[String] = (valueBytes: Array[Byte]) => new String(valueBytes).split(' ').toSet

  protected def apply(dataSource: DataSource): KnownNodesStorage = new KnownNodesStorage(dataSource)

  def getKnownNodes(): Set[URI] = {
    get(key).getOrElse(Set.empty).filter(_.nonEmpty).map(new URI(_))
  }

  def updateKnownNodes(toAdd: Set[URI] = Set.empty, toRemove: Set[URI] = Set.empty): KnownNodesStorage = {
    val updated = (getKnownNodes() ++ toAdd) -- toRemove
    put(key, updated.map(_.toString))
  }

}
