package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingRootNodeException
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.mpt.MptTraversals
import io.iohk.ethereum.mpt.NodesKeyValueStorage

trait MptStorage {
  def get(nodeId: Array[Byte]): MptNode
  def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode]
  def persist(): Unit
}

class SerializingMptStorage(storage: NodesKeyValueStorage) extends MptStorage {
  override def get(nodeId: Array[Byte]): MptNode = {
    val key = ByteString(nodeId)
    storage
      .get(key)
      .map(nodeEncoded => MptStorage.decodeNode(nodeEncoded, nodeId))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    val (collapsed, toUpdate) = MptStorage.collapseNode(newRoot)
    val toBeRemoved = toRemove.map(n => ByteString(n.hash))
    storage.update(toBeRemoved, toUpdate)
    collapsed
  }

  override def persist(): Unit =
    storage.persist()
}

object MptStorage {
  def collapseNode(node: Option[MptNode]): (Option[MptNode], List[(ByteString, Array[Byte])]) =
    if (node.isEmpty)
      (None, List.empty[(ByteString, Array[Byte])])
    else {
      val (hashNode, newNodes) = MptTraversals.collapseTrie(node.get)
      (Some(hashNode), newNodes)
    }

  def decodeNode(nodeEncoded: NodeEncoded, nodeId: Array[Byte]): MptNode =
    MptTraversals.decodeNode(nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded)
}
