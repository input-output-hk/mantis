package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage._

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  *   Key: hash of the RLP encoded node
  *   Value: the RLP encoded node
  */
class NodeStorage(val dataSource: DataSource) {

  val namespace: IndexedSeq[Byte] = Namespaces.NodeNamespace
  private val specialNameSpace = namespace.head

  def specialSerializer(nodeHash: NodeHash): Array[Byte] = {
    (specialNameSpace +: nodeHash).toArray
  }

  def get(key: NodeHash): Option[NodeEncoded] = dataSource.getOptimized(specialSerializer(key))

  /**
    * This function updates the KeyValueStorage by deleting, updating and inserting new (key-value) pairs
    * in the current namespace.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStorage.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStorage.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new KeyValueStorage after the removals and insertions were done.
    */
  def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodeStorage = {
    val newDataSource = dataSource.updateOptimized(
      toRemove = toRemove.map(specialSerializer),
      toUpsert = toUpsert.map(values => specialSerializer(values._1) -> values._2)
    )
    apply(newDataSource)
  }

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)
}

object NodeStorage {
  type NodeHash = ByteString
  type NodeEncoded = Array[Byte]
}
