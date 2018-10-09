package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingRootNodeException

trait StateStorage {

}

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