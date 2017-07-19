package io.iohk.ethereum.db.storage

import java.net.URI

import io.iohk.ethereum.db.dataSource.DataSource

/**
  * This class is used to store discovered nodes
  *   Value: stored nodes list
  */
class KnownNodesStorage(val dataSource: DataSource) extends KeyValueStorage[String, Seq[String], KnownNodesStorage]{
  type T = KnownNodesStorage

  val separator = "|"
  val key = "KnownNodes"

  val namespace: IndexedSeq[Byte] = Namespaces.KnownNodesNamespace
  def keySerializer: String => IndexedSeq[Byte] = _.getBytes
  def valueSerializer: Seq[String] => IndexedSeq[Byte] = _.mkString(separator).getBytes
  def valueDeserializer: IndexedSeq[Byte] => Seq[String] = (valueBytes: IndexedSeq[Byte]) => new String(valueBytes.toArray).split(separator)

  protected def apply(dataSource: DataSource): KnownNodesStorage = new KnownNodesStorage(dataSource)

  def getKnownNodes(): Seq[URI] = {
    get(key).getOrElse(Nil).map(uriFromString)
  }

  def removeKnownNode(node: URI): KnownNodesStorage = {
    val newNodes = getKnownNodes().filter(u => u.getHost == node.getHost && u.getPort == node.getPort)
    put(key, newNodes.map(uriToString))
  }

  private def uriFromString(enodeStr: String): URI = {
    new URI(enodeStr)
  }

  private def uriToString(uri: URI): String = {
    s"${uri.getUserInfo}@${uri.getHost}:${uri.getPort}"
  }

  def addKnownNode(node: URI): KnownNodesStorage = {
    val newNodes = getKnownNodes() :+ node
    put(key, newNodes.map(uriToString))
  }
}
