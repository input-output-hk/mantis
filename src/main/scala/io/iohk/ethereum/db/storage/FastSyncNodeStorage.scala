package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import encoding._
import io.iohk.ethereum.db.storage.pruning.PruneSupport
import io.iohk.ethereum.utils.Logger
/**
  * This class is specialization of ReferenceCountNodeStorage.
  * It Uses the same serialization format as ReferenceCountNodeStorage, but omits all logic regarding reference counting.
  * It is possible to do that as during FastSyncing we are saving every mpt node under one block (one mpt trie), so every
  * node saved will have its reference count equal to 1.
  *
  * */
class FastSyncNodeStorage(nodeStorage: NodesStorage, blockNumber: Option[BigInt] = None)
  extends ReferenceCountNodeStorage(nodeStorage, blockNumber) {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] = nodeStorage.get(key).map(storedNodeFromBytes).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    require(blockNumber.isDefined)

    val bn = blockNumber.get

    val toUpsertUpdated = toUpsert.map {item =>
      val (nodeKey, nodeEncoded) = item
      nodeKey -> storedNodeToBytes(StoredNode.withoutReferences(nodeEncoded).incrementReferences(1, bn))
    }

    nodeStorage.updateCond(toRemove, toUpsertUpdated, inMemory = true)
    this
  }

}

object FastSyncNodeStorage extends PruneSupport with Logger {
  /**
    * It should not be used, as this specialization does not reference count. It is No-op.
    * @param blockNumber BlockNumber to prune
    * @param nodeStorage NodeStorage
    */
  override def prune(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = ()
  /**
    * It should not be used, as this specialization does not reference count. It is No-op.
    *
    * @param blockNumber BlockNumber to rollback
    * @param nodeStorage NodeStorage
    */
  override def rollback(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = ()
}
