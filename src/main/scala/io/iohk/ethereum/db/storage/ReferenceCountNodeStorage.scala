package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{PruneResult, PruningNodesKeyValueStorage, RangePrune}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import encoding._

/**
  * This class helps to deal with two problems regarding MptNodes storage:
  * 1) Define a way to delete ones that are no longer needed
  * 2) Avoids removal of nodes that can be used in diferent trie branches because the hash it's the same
  *
  * To deal with (1) when a node is no longer needed it's tagged with the corresponding block number in order to be
  * able to release disk space used (as it's not going to be accessed any more)
  *
  * In order to solve (2), before saving a node, its wrapped with the number of references it has. The inverse operation
  * is done when getting a node.
  *
  * Using this storage will change data to be stored in nodeStorage in two ways (and it will, as consequence, make
  * different pruning mechanisms incompatible):
  * - Instead of saving KEY -> VALUE, it will store KEY -> (VALUE, REFERENCE_COUNT)
  * - Also, the following index will be appended: BLOCK_NUMBER_TAG -> Seq(KEY1, KEY2, ..., KEYn)
  */
class ReferenceCountNodeStorage(nodeStorage: NodeStorage, pruningOffset: BigInt, blockNumber: Option[BigInt] = None)
  extends PruningNodesKeyValueStorage
  with RangePrune {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] = nodeStorage.get(key).map(storedNodeFromBytes).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {

    require(blockNumber.isDefined)

    val bn = blockNumber.get
    // Process upsert changes. As the same node might be changed twice within the same update, we need to keep changes
    // within a map
    val upsertChanges = toUpsert.foldLeft(Map.empty[NodeHash, (StoredNode, StoredNode)]) { (storedNodes, toUpsertItem) =>
      val (nodeKey, nodeEncoded) = toUpsertItem
      val (storedNode, snapshot) = storedNodes.get(nodeKey) // get from current changes
        .orElse(nodeStorage.get(nodeKey).map(storedNodeFromBytes).map(s => s -> s)) // or get from DB
        .getOrElse(StoredNode(ByteString(nodeEncoded), 0) -> StoredNode(ByteString(nodeEncoded), 0)) // if it's new, return an empty stored node

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
        case ((toRemoveUpdated, toUpsertUpdated, previous), (key, (storedNode, snapshot))) =>
          if(storedNode.references == 0) (toRemoveUpdated :+ key, toUpsertUpdated, previous :+ (key -> snapshot))
          else (toRemoveUpdated, toUpsertUpdated :+ (key -> storedNodeToBytes(storedNode)), previous :+ (key -> snapshot))
      }

    val snapshotToSave = {
      if(snapshot.nonEmpty) {
        // If non empty, try append the new values to the ones stored in DB
        val key = snapshotKey(bn)
        val updated = nodeStorage.get(key).map(storedNodesFromBytes).getOrElse(Seq.empty) ++ snapshot
        (key -> storedNodesToBytes(updated)) :: Nil
      }
      else Nil
    }

    nodeStorage.update(toRemoveUpdated, toUpsertUpdated ++ snapshotToSave)

    this
  }

  /**
    * Determines and prunes mpt nodes based on last pruned block number tag and the current best block number
    *
    * @param lastPruned      Last pruned block number tag
    * @param bestBlockNumber Current best block number
    * @return PruneResult
    */
  override def prune(lastPruned: => BigInt, bestBlockNumber: => BigInt): PruneResult = {
    val from = lastPruned + 1
    val to = from.max(bestBlockNumber - pruningOffset)
    pruneBetween(from, to, bn => ReferenceCountNodeStorage.prune(bn, nodeStorage))
  }

  override def rollbackChanges(blockNumber: BigInt): Unit = {
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
        nodeStorage.update(toRemove :+ theSnapshotKey, toUpsert) // remove snapshot as we have done a rollback
      }
  }
}

object ReferenceCountNodeStorage {

  /**
    * Based on a block number tag, looks for no longer needed nodes and deletes them if it corresponds (a node that was
    * marked as unused in a certain block number tag, might be used later)
    *
    * @param blockNumber
    * @param nodeStorage
    * @return
    */
  private def prune(blockNumber: BigInt, nodeStorage: NodeStorage): Unit =
    nodeStorage.remove(snapshotKey(blockNumber))

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

  /**
    * Key to be used to store PruneCandidates index. PruneKey -> PruneCandidates
    *
    * @param blockNumber Block Number Tag
    * @return Key
    */
  private def snapshotKey(blockNumber: BigInt): ByteString = ByteString('a'.toByte +: blockNumber.toByteArray)

}
