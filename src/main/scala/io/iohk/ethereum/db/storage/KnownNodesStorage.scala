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
  def keySerializer: String => IndexedSeq[Byte] = _.getBytes
  def valueSerializer: Set[String] => IndexedSeq[Byte] = _.mkString(" ").getBytes
  def valueDeserializer: IndexedSeq[Byte] => Set[String] = (valueBytes: IndexedSeq[Byte]) => new String(valueBytes.toArray).split(' ').toSet

  protected def apply(dataSource: DataSource): KnownNodesStorage = new KnownNodesStorage(dataSource)

  def getKnownNodes(): Set[URI] = {
    get(key).getOrElse(Set.empty).filter(_.nonEmpty).map(new URI(_))
  }

  def updateKnownNodes(toAdd: Set[URI] = Set.empty, toRemove: Set[URI] = Set.empty): KnownNodesStorage = {
    val updated = getKnownNodes() -- toRemove ++ toAdd
    put(key, updated.map(_.toString))
  }

}
