package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodesStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingRootNodeException
import scala.collection.mutable


trait MptStorage {
  def get(nodeId: Array[Byte]): MptNode
  def updateNodesInStorage( newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode]
}

class SimpleMptStorage(source: NodesKeyValueStorage) extends MptStorage {
  override def get(nodeId: Array[Byte]): MptNode = {
    source.get(ByteString(nodeId))
      .map(nodeEncoded => MptTraversals.decodeNode(nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    val (collapsed, toUpdate) =
      if (newRoot.isEmpty)
        (None, List.empty[(ByteString, Array[Byte])])
      else {
        val (hashNode, newNodes) = MptTraversals.collapseTrie(newRoot.get)
        (Some(hashNode), newNodes)
      }

    val toBeRemoved = toRemove.map(n => ByteString(n.hash))
    source.update(toBeRemoved, toUpdate)
    collapsed
  }
}

class ExperimentalStorage(nodesStorage: NodesStorage, getNode: ByteString => Option[Array[Byte]]) extends MptStorage {
  private val buffer = mutable.Map.empty[ByteString, Option[Array[Byte]]]

  private def changes = buffer.foldLeft(Seq.empty[ByteString] -> Seq.empty[(ByteString, Array[Byte])]) { (acc, cachedItem) =>
    cachedItem match {
      case (key, Some(value)) => (acc._1, acc._2 :+ key -> value)
      case (key, None) => (acc._1 :+ key, acc._2)
    }
  }

  override def get(nodeId: Array[Byte]): MptNode = {
    val key = ByteString(nodeId)
    buffer
      .getOrElse(key, getNode(key))
      .map(nodeEncoded => MptTraversals.decodeNode(nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    val (collapsed, toUpdate) =
      if (newRoot.isEmpty)
        (None, List.empty[(ByteString, Array[Byte])])
      else {
        val (hashNode, newNodes) = MptTraversals.collapseTrie(newRoot.get)
        (Some(hashNode), newNodes)
      }

    toUpdate.foreach(n => buffer += (n._1 -> Some(n._2)))
    toRemove.foreach(n => buffer += (ByteString(n.hash) -> None))

    collapsed
  }

  def persist(storage: NodesKeyValueStorage): Unit = {
    val allchanges = changes
    println(s"Flushing buffer, there are ${allchanges._1.size} deletes, and ${allchanges._2.size} updates")
    storage.update(allchanges._1, allchanges._2)
    buffer.clear()
  }
}