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
  * In order to solve (2), before saving a node, its wrapped with the number of references it has.
  *
  * Using this storage will change data to be stored in nodeStorage in two ways (and it will, as consequence, make
  * different pruning mechanisms incompatible):
  * - Instead of saving KEY -> VALUE, it will store KEY -> STORED_NODE(VALUE, REFERENCE_COUNT)
  * - Also, the following index will be appended: BLOCK_NUMBER_TAG -> NUMBER_OF_SNAPSHOTS
  * - All the snapshots as: (BLOCK_NUMBER_TAG ++ SNAPSHOT_INDEX) -> SNAPSHOT
  */
class ReferenceCountNodeStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt] = None)
  extends NodesKeyValueStorage {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] = nodeStorage.get(key).map(storedNodeFromBytes).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {

    require(blockNumber.isDefined)

    val bn = blockNumber.get
    // Process upsert changes. As the same node might be changed twice within the same update, we need to keep changes
    // within a map. There is also stored the snapshot version before changes
    val upsertChanges = applyUpsertChanges(toUpsert)
    val changes = applyRemovalChanges(toRemove, upsertChanges)

    val (toRemoveUpdated, toUpsertUpdated, snapshots) =
      changes.foldLeft(Seq.empty[NodeHash], Seq.empty[(NodeHash, NodeEncoded)], Seq.empty[StoredNodeSnapshot]) {
        case ((removeAcc, upsertAcc, snapshotAcc), (key, (storedNode, theSnapshot))) =>
          // If no more references, move it to the list to be removed
          if (storedNode.references == 0) (removeAcc :+ key, upsertAcc, snapshotAcc :+ theSnapshot)
          else (removeAcc, upsertAcc :+ (key -> storedNodeToBytes(storedNode)), snapshotAcc :+ theSnapshot)
      }

    val snapshotToSave: Seq[(NodeHash, Array[Byte])] = getSnapshotsToSave(bn, snapshots)
    nodeStorage.update(toRemoveUpdated, toUpsertUpdated ++ snapshotToSave)
    this
  }

  private def applyUpsertChanges(toUpsert: Seq[(NodeHash, NodeEncoded)]): Changes =
    toUpsert.foldLeft(Map.empty[NodeHash, (StoredNode, StoredNodeSnapshot)]) { (storedNodes, toUpsertItem) =>
      val (nodeKey, nodeEncoded) = toUpsertItem
      val (storedNode, snapshot) = getFromChangesOrStorage(nodeKey, storedNodes)
        .getOrElse(StoredNode.withoutReferences(nodeEncoded) -> StoredNodeSnapshot(nodeKey, None)) // if it's new, return an empty stored node

      storedNodes + (nodeKey -> (storedNode.incrementReferences(1), snapshot))
    }

  private def applyRemovalChanges(toRemove: Seq[NodeHash], changes: Map[NodeHash, (StoredNode, StoredNodeSnapshot)]): Changes =
    toRemove.foldLeft(changes) { (storedNodes, nodeKey) =>
      val maybeStoredNode: Option[(StoredNode, StoredNodeSnapshot)] = getFromChangesOrStorage(nodeKey, storedNodes)

      maybeStoredNode.fold(storedNodes) {
        case (storedNode, snapshot) => storedNodes + (nodeKey -> (storedNode.decrementReferences(1), snapshot))
      }
    }

  private def getSnapshotsToSave(blockNumber: BigInt, snapshots: Seq[StoredNodeSnapshot]): Seq[(NodeHash, Array[Byte])] =
    if(snapshots.nonEmpty) {
      // If not empty, snapshots will be stored indexed by block number and index
      val snapshotCountKey = getSnapshotsCountKey(blockNumber)
      val getSnapshotKeyFn = getSnapshotKey(blockNumber)(_)
      val blockNumberSnapshotsCount: BigInt = nodeStorage.get(snapshotCountKey).map(snapshotsCountFromBytes).getOrElse(0)
      val snapshotsToSave = snapshots.zipWithIndex.map { case (snapshot, index) =>
        getSnapshotKeyFn(blockNumberSnapshotsCount + index + 1) -> snapshotToBytes(snapshot)
      }
      // Save snapshots and latest snapshot index
      (snapshotCountKey -> snapshotsCountToBytes(blockNumberSnapshotsCount + snapshotsToSave.size)) +: snapshotsToSave
    }
    else Nil

  private def getFromChangesOrStorage(nodeKey: NodeHash, storedNodes: Changes): Option[(StoredNode, StoredNodeSnapshot)] =
    storedNodes
      .get(nodeKey)
      .orElse(nodeStorage.get(nodeKey).map(storedNodeFromBytes).map(sn => sn -> StoredNodeSnapshot(nodeKey, Some(sn))))
}

object ReferenceCountNodeStorage extends PruneSupport with Logger {

  type Changes = Map[NodeHash, (StoredNode, StoredNodeSnapshot)]

  /**
    * Removes snapshots stored in the DB that are not longer needed, which means, cannot be rolled back to
    *
    * @param blockNumber BlockNumber to prune
    * @param nodeStorage NodeStorage
    */
  override def prune(blockNumber: BigInt, nodeStorage: NodeStorage): Unit = {
    log.debug(s"Pruning block $blockNumber")

    withSnapshotCount(blockNumber, nodeStorage) { (snapshotsCountKey, snapshotCount) =>
      val snapshotKeys = snapshotKeysUpTo(blockNumber, snapshotCount)
      nodeStorage.update(snapshotsCountKey +: snapshotKeys, Nil)
    }

    log.debug(s"Pruned block $blockNumber")
  }

  /**
    * Looks for the StoredNode snapshots based on block number and saves (or deletes) them
    *
    * @param blockNumber BlockNumber to rollback
    * @param nodeStorage NodeStorage
    */
  override def rollback(blockNumber: BigInt, nodeStorage: NodeStorage): Unit =
    withSnapshotCount(blockNumber, nodeStorage) { (snapshotsCountKey, snapshotCount) =>
      // Get all the snapshots
      val snapshots = snapshotKeysUpTo(blockNumber, snapshotCount)
        .flatMap(key => nodeStorage.get(key).map(snapshotFromBytes))

      // Transform them to db operations
      val (toRemove, toUpsert) = snapshots.foldLeft(Seq.empty[NodeHash], Seq.empty[(NodeHash, NodeEncoded)]) {
        // Undo Actions
        case ((r, u), StoredNodeSnapshot(nodeHash, Some(sn))) => (r, (nodeHash -> storedNodeToBytes(sn)) +: u)
        case ((r, u), StoredNodeSnapshot(nodeHash, None)) => (nodeHash +: r, u)
      }
      // also remove snapshot as we have done a rollback
      nodeStorage.update(toRemove :+ snapshotsCountKey, toUpsert)
    }

  private def withSnapshotCount(blockNumber: BigInt, nodeStorage: NodeStorage)(f: (ByteString, BigInt) => Unit): Unit = {
    val snapshotsCountKey = getSnapshotsCountKey(blockNumber)
    // Look for snapshot count for given block number
    val maybeSnapshotCount = nodeStorage.get(snapshotsCountKey).map(snapshotsCountFromBytes)
    maybeSnapshotCount match {
      case Some(snapshotCount) => f(snapshotsCountKey, snapshotCount)
      case None => ()
    }
  }

  private def snapshotKeysUpTo(blockNumber: BigInt, snapshotCount: BigInt): Seq[ByteString] = {
    val getSnapshotKeyFn = getSnapshotKey(blockNumber)(_)
    (BigInt(0) to snapshotCount).map(snapshotIndex => getSnapshotKeyFn(snapshotIndex))
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
    * Key to be used to store BlockNumber -> Snapshots Count
    *
    * @param blockNumber Block Number Tag
    * @return Key
    */
  private def getSnapshotsCountKey(blockNumber: BigInt): ByteString = ByteString("sck".getBytes ++ blockNumber.toByteArray)

  /**
    * Returns a snapshot key given a block number and a snapshot index
    * @param blockNumber Block Number Ta
    * @param index Snapshot Index
    * @return
    */
  private def getSnapshotKey(blockNumber: BigInt)(index: BigInt): ByteString = ByteString(("sk".getBytes ++ blockNumber.toByteArray) ++ index.toByteArray)

  /**
    * Used to store a node snapshot in the db. This will be used to rollback a transaction.
    * @param nodeKey Node's key
    * @param storedNode Stored node that can be rolledback. If None, it means that node wasn't previously in the DB
    */
  case class StoredNodeSnapshot(nodeKey: NodeHash, storedNode: Option[StoredNode])

}
