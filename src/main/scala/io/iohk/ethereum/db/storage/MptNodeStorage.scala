package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

import MptNodeStorage._

/**
  * This class is used to store MptNodes, by using:
  *   Key: hash of the RLP encoded node
  *   Value: the node
  */
class MptNodeStorage(val dataSource: DataSource) extends KeyValueStorage[MptNodeHash, MptNode] {
  type T = MptNodeStorage

  val namespace: IndexedSeq[Byte] = Namespaces.NodeNamespace
  def keySerializer: MptNodeHash => IndexedSeq[Byte] = identity
  def valueSerializer: MptNode => IndexedSeq[Byte] = (node: MptNode) => rlpEncode(node).toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => MptNode = (encodedNode: IndexedSeq[Byte]) => rlpDecode[MptNode](encodedNode.toArray)

  protected def apply(dataSource: DataSource): MptNodeStorage = new MptNodeStorage(dataSource)

  /**
    * This function updates the MptNodeStorage by inserting the node in the node namespace.
    *
    * @param node to be inserted, from which the key calculation is done.
    * @return the new MptNodeStorage after the insertion was done.
    */
  def put(node: MptNode): MptNodeStorage = put(node.hash, node)
}

object MptNodeStorage {
  type MptNodeHash = ByteString
}
