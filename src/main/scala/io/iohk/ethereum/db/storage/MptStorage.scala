package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingRootNodeException
import io.iohk.ethereum.mpt.{HashNode, MptNode, MptTraversals, NodesKeyValueStorage}

trait MptStorage {
  def get(nodeId: Array[Byte]): MptNode
  def updateNodesInStorage( newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode]
  def persist(): Unit
}

class SerializingMptStorage(storage: NodesKeyValueStorage) extends MptStorage {
  override def get(nodeId: Array[Byte]): MptNode = {
    val key = ByteString(nodeId)
    storage.get(key)
      .map(nodeEncoded => MptStorage.decodeNode(nodeEncoded, nodeId))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    val (collapsed, toUpdate) = MptStorage.collapseNode(newRoot)
    val toBeRemoved = toRemove.map(n => ByteString(n.hash))
    storage.update(toBeRemoved, toUpdate)
    collapsed
  }

  override def persist(): Unit = {
    storage.persist()
  }
}

class TracingMptStorage(tracingBuffer: TracingBuffer) extends MptStorage {
  override def get(nodeId: Array[Byte]): MptNode = {
    val key = ByteString(nodeId)
    tracingBuffer.get(key)
      .map(nodeEncoded => MptStorage.decodeNode(nodeEncoded, nodeId))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    if (newRoot.isEmpty) {
      None
    } else {
      val (rootNode, toUpdate) = MptTraversals.getCached(newRoot.get)
      val toBeRemoved = toRemove.map(n => ByteString(n.hash))
      tracingBuffer.update(toUpdate.map(node => (node.nodeHash, node.nodeEncoded)), toBeRemoved.toList, rootNode)
      Some(HashNode(rootNode.hash.toArray[Byte]))
    }
  }

  override def persist(): Unit = {

  }
}

object MptStorage {
  def collapseNode(node: Option[MptNode]): (Option[MptNode], List[(ByteString, Array[Byte])]) = {
    if (node.isEmpty)
      (None, List.empty[(ByteString, Array[Byte])])
    else {
      val (hashNode, newNodes) = MptTraversals.collapseTrie(node.get)
      (Some(hashNode), newNodes)
    }
  }

  def decodeNode(nodeEncoded: NodeEncoded, nodeId: Array[Byte]): MptNode = {
    MptTraversals.decodeNode(nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded)
  }
}
