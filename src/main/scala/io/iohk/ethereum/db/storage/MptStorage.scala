package io.iohk.ethereum.db.storage

import akka.util.ByteString

import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingRootNodeException
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.mpt.MptTraversals

/** Storage of object nodes, agnostic of materialization */
sealed trait MptStorage {
  def get(nodeId: Array[Byte]): MptNode
  def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode]
  def persist(): Unit
}

/** Binds materialized and abstract node storage
  * @param storage materialized storage
  */
class SerializingMptStorage(storage: NodesKeyValueStorage) extends MptStorage {
  override def get(nodeId: Array[Byte]): MptNode = {
    val key = ByteString(nodeId)
    storage
      .get(key)
      .map(nodeEncoded => SerializingMptStorage.decodeNode(nodeEncoded, nodeId))
      .getOrElse(throw new MissingRootNodeException(ByteString(nodeId)))
  }

  override def updateNodesInStorage(newRoot: Option[MptNode], toRemove: Seq[MptNode]): Option[MptNode] = {
    val (collapsed, toUpdate) = SerializingMptStorage.collapseNode(newRoot)
    val toBeRemoved = toRemove.map(n => ByteString(n.hash))
    storage.update(toBeRemoved, toUpdate)
    collapsed
  }

  override def persist(): Unit =
    storage.persist()
}

object SerializingMptStorage {
  private[storage] def collapseNode(node: Option[MptNode]): (Option[MptNode], List[(ByteString, Array[Byte])]) =
    node match {
      case Some(newRoot) =>
        val (hashNode, newNodes) = MptTraversals.collapseTrie(newRoot)
        (Some(hashNode), newNodes)
      case None =>
        (None, List.empty[(ByteString, Array[Byte])])
    }

  private[storage] def decodeNode(nodeEncoded: NodeEncoded, nodeId: Array[Byte]): MptNode =
    MptTraversals.decodeNode(nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded)
}
