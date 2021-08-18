package io.iohk.ethereum.db.storage

import akka.util.ByteString

import io.iohk.ethereum.db.storage.StorageTypes.NodeEncoded
import io.iohk.ethereum.db.storage.StorageTypes.NodeHash
import io.iohk.ethereum.db.storage.encoding._

/** This class is specialization of ReferenceCountNodeStorage.
  * It Uses the same serialization format as ReferenceCountNodeStorage, but omits all logic regarding reference counting.
  * It is possible to do that as during FastSyncing we are saving every mpt node under one block (one mpt trie), so every
  * node saved will have its reference count equal to 1.
  */
class FastSyncNodeStorage(nodeStorage: NodesStorage, bn: BigInt) extends ReferenceCountNodeStorage(nodeStorage, bn) {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] =
    nodeStorage.get(key).map(storedNodeFromBytes).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): FastSyncNodeStorage = {
    val toUpsertUpdated = toUpsert.map { item =>
      val (nodeKey, nodeEncoded) = item
      nodeKey -> storedNodeToBytes(StoredNode.withoutReferences(nodeEncoded).incrementReferences(1, bn))
    }

    nodeStorage.updateCond(toRemove, toUpsertUpdated, inMemory = true)
    this
  }

  override def persist(): Unit = {}
}
