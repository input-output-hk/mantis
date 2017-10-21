package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import encoding._
import io.iohk.ethereum.db.storage.pruning.PruneSupport
import io.iohk.ethereum.utils.Logger

/**
  * This class helps to deal with two problems regarding MptNodes storage:
  * 1) Define a way to delete ones that are no longer needed but allow rollbacks to be performed
  * 2) Avoids removal of nodes that can be used in different trie branches because the hash it's the same
  *
  * To deal with (1) when a node is no longer needed, block number alongside with a stored node snapshot is saved so
  * it can be restored in case of rollback
  *
  * In order to solve (2), before saving a node, its wrapped with the number of references it has. The inverse operation
  * is done when getting a node.
  *
  * Using this storage will change data to be stored in nodeStorage in two ways (and it will, as consequence, make
  * different pruning mechanisms incompatible):
  * - Instead of saving KEY -> VALUE, it will store STORED_NODE = KEY -> (VALUE, REFERENCE_COUNT)
  * - Also, the following index will be appended: BLOCK_NUMBER_TAG -> Seq(STORED_NODE1,..., STORED_NODEn)
  */
class ReferenceCountNodeStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt] = None)
  extends NodesKeyValueStorage {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] = nodeStorage.get(key).map(storedNodeFromBytes).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {

    require(blockNumber.isDefined)

    val bn = blockNumber.get
    // Process upsert changes. As the same node might be changed twice within the same update, we need to keep changes
    // within a map. There is also stored the snapshot version ebfore changes
    val upsertChanges = toUpsert.foldLeft(Map.empty[NodeHash, (StoredNode, StoredNode)]) { (storedNodes, toUpsertItem) =>
      val (nodeKey, nodeEncoded) = toUpsertItem
      val (storedNode, snapshot) = storedNodes.get(nodeKey) // get from current changes
        .orElse(nodeStorage.get(nodeKey).map(storedNodeFromBytes).map(sn => sn -> sn)) // or get from DB
        .getOrElse(StoredNode.withoutReferences(nodeEncoded) -> StoredNode.withoutReferences(nodeEncoded)) // if it's new, return an empty stored node

      storedNodes + (nodeKey -> (storedNode.incrementReferences(1), snapshot))
    }

    val changes = toRemove.foldLeft(upsertChanges) { (storedNodes, nodeKey) =>
      val maybeStoredNode: Option[(StoredNode, StoredNode)] = storedNodes.get(nodeKey) // get from current changes
        .orElse(nodeStorage.get(nodeKey).map(storedNodeFromBytes).map(s => s -> s)) // or db

      if (maybeStoredNode.isDefined) {
        val (storedNode, snapshot) = maybeStoredNode.get
        storedNodes + (nodeKey -> (storedNode.decrementReferences(1), snapshot))
      }
      else storedNodes
    }

    val (toRemoveUpdated, toUpsertUpdated, snapshot) =
      changes.foldLeft(Seq.empty[NodeHash], Seq.empty[(NodeHash, NodeEncoded)], Seq.empty[(NodeHash, StoredNode)]) {
        case ((removeAcc, upsertAcc, snapshotAcc), (key, (storedNode, theSnapshot))) =>
          // If no more references, move it to the list to be removed
          if(storedNode.references == 0) (removeAcc :+ key, upsertAcc, snapshotAcc :+ (key -> theSnapshot))
          else (removeAcc, upsertAcc :+ (key -> storedNodeToBytes(storedNode)), snapshotAcc :+ (key -> theSnapshot))
      }

    val snapshotToSave = {
      if(snapshot.nonEmpty) {
        // If non empty, try append the new node snapshots to the ones stored in DB
        val key = snapshotKey(bn)
        val updated = nodeStorage.get(key).map(storedNodesFromBytes).getOrElse(Seq.empty) ++ snapshot
        (key -> storedNodesToBytes(updated)) :: Nil
      }
      else Nil
    }

    nodeStorage.update(toRemoveUpdated, toUpsertUpdated ++ snapshotToSave)

    this
  }
}

object ReferenceCountNodeStorage extends PruneSupport with Logger {

  /**
    * Removes snapshots stored in the DB that are not longer needed, which means, cannot be rolled back to
    * @param blockNumber BlockNumber to prune
    * @param nodeStorage NodeStorage
    */
  override def prune(blockNumber: BigInt, nodeStorage: NodeStorage): Unit = {
    log.debug(s"Pruning block $blockNumber")
    nodeStorage.remove(snapshotKey(blockNumber))
    log.debug(s"Pruned block $blockNumber")
  }

  /**
    * Looks for the StoredNode snapshots based on block number and saves (or deletes) them
    *
    * @param blockNumber BlockNumber to rollback
    * @param nodeStorage NodeStorage
    */
  override def rollback(blockNumber: BigInt, nodeStorage: NodeStorage): Unit = {
    val theSnapshotKey = snapshotKey(blockNumber)
    nodeStorage
      .get(theSnapshotKey)
      .map(storedNodesFromBytes)
      .map { snapshot =>
        val (toRemove, toUpsert) = snapshot.foldLeft(Seq.empty[NodeHash], Seq.empty[(NodeHash, NodeEncoded)]) {
          // Undo Actions
          case((r, u), (nodeHash, sn)) if sn.references > 0 => (r, (nodeHash -> storedNodeToBytes(sn)) +: u)
          case((r, u), (nodeHash, sn)) if sn.references <= 0 => (nodeHash +: r, u)
        }
        // also remove snapshot as we have done a rollback
        nodeStorage.update(toRemove :+ theSnapshotKey, toUpsert)
      }
  }


  /**
    * Wrapper of MptNode in order to store number of references it has.
    *
    * @param nodeEncoded Encoded Mpt Node to be used in MerklePatriciaTrie
    * @param references  Number of references the node has. Each time it's updated references are increased and everytime it's deleted, decreased
    */
  case class StoredNode(nodeEncoded: ByteString, references: Int) {
    def incrementReferences(amount: Int): StoredNode = copy(references = references + amount)

    def decrementReferences(amount: Int): StoredNode = copy(references = references - amount)
  }

  object StoredNode {
    def withoutReferences(nodeEncoded: Array[Byte]): StoredNode = new StoredNode(ByteString(nodeEncoded), 0)
  }

  /**
    * Key to be used to store PruneCandidates index. PruneKey -> PruneCandidates
    *
    * @param blockNumber Block Number Tag
    * @return Key
    */
  private def snapshotKey(blockNumber: BigInt): ByteString = ByteString('a'.toByte +: blockNumber.toByteArray)

}
