package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.pruning.PruneSupport
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Logger

import encoding._

/** This class helps to deal with two problems regarding MptNodes storage:
  * 1) Define a way to delete ones that are no longer needed but allow rollbacks to be performed
  * 2) Avoids removal of nodes that can be used in different trie branches because the hash is the same
  *
  * To deal with (1) when a node is no longer needed, block number alongside with a stored node snapshot is saved so
  * it can be restored in case of rollback.
  *
  * In order to solve (2), before saving a node, it's wrapped with the number of references it has.
  *
  * Using this storage will change data to be stored in nodeStorage in two ways (and it will, as consequence, make
  * different pruning mechanisms incompatible):
  * - Instead of saving KEY -> VALUE, it will store KEY -> STORED_NODE(VALUE, REFERENCE_COUNT, LAST_USED_BY_BLOCK)
  *
  * Also, additional data will be saved in this storage:
  * - For each block: BLOCK_NUMBER_TAG -> NUMBER_OF_SNAPSHOTS
  * - For each node changed within a block: (BLOCK_NUMBER_TAG ++ SNAPSHOT_INDEX) -> SNAPSHOT
  *
  * Storing snapshot info this way allows for easy construction of snapshot key (based on a block number
  * and number of snapshots) and therefore, fast access to each snapshot individually.
  */
class ReferenceCountNodeStorage(nodeStorage: NodesStorage, bn: BigInt) extends NodesKeyValueStorage {

  import ReferenceCountNodeStorage._

  def get(key: ByteString): Option[NodeEncoded] =
    nodeStorage.get(key).map(node => storedNodeFromBytes(node).nodeEncoded.toArray)

  def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): ReferenceCountNodeStorage = {

    val deathRowKey = drRowKey(bn)

    var currentDeathRow = getDeathRow(deathRowKey, nodeStorage)
    // Process upsert changes. As the same node might be changed twice within the same update, we need to keep changes
    // within a map. There is also stored the snapshot version before changes
    val upsertChanges = prepareUpsertChanges(toUpsert, bn)
    val changes = prepareRemovalChanges(toRemove, upsertChanges, bn)

    val (toUpsertUpdated, snapshots) =
      changes.foldLeft(Seq.empty[(NodeHash, NodeEncoded)], Seq.empty[StoredNodeSnapshot]) {
        case ((upsertAcc, snapshotAcc), (key, (storedNode, theSnapshot))) =>
          // Update it in DB

          // if after update number references drop to zero mark node as possible for deletion after x blocks
          if (storedNode.references == 0) {
            currentDeathRow = currentDeathRow ++ key
          }

          (upsertAcc :+ (key -> storedNodeToBytes(storedNode)), snapshotAcc :+ theSnapshot)
      }

    val snapshotToSave: Seq[(NodeHash, Array[Byte])] = getSnapshotsToSave(bn, snapshots)

    val deathRow =
      if (currentDeathRow.nonEmpty)
        Seq(deathRowKey -> currentDeathRow.toArray[Byte])
      else
        Seq()

    nodeStorage.updateCond(Nil, deathRow ++ toUpsertUpdated ++ snapshotToSave, inMemory = true)
    this
  }

  override def persist(): Unit = {}

  private def prepareUpsertChanges(toUpsert: Seq[(NodeHash, NodeEncoded)], blockNumber: BigInt): Changes =
    toUpsert.foldLeft(Map.empty[NodeHash, (StoredNode, StoredNodeSnapshot)]) { (storedNodes, toUpsertItem) =>
      val (nodeKey, nodeEncoded) = toUpsertItem
      val (storedNode, snapshot) = getFromChangesOrStorage(nodeKey, storedNodes)
        .getOrElse(
          StoredNode.withoutReferences(nodeEncoded) -> StoredNodeSnapshot(nodeKey, None)
        ) // if it's new, return an empty stored node

      storedNodes + (nodeKey -> (storedNode.incrementReferences(1, blockNumber), snapshot))
    }

  private def prepareRemovalChanges(
      toRemove: Seq[NodeHash],
      changes: Map[NodeHash, (StoredNode, StoredNodeSnapshot)],
      blockNumber: BigInt
  ): Changes =
    toRemove.foldLeft(changes) { (storedNodes, nodeKey) =>
      val maybeStoredNode: Option[(StoredNode, StoredNodeSnapshot)] = getFromChangesOrStorage(nodeKey, storedNodes)

      maybeStoredNode.fold(storedNodes) { case (storedNode, snapshot) =>
        storedNodes + (nodeKey -> (storedNode.decrementReferences(1, blockNumber), snapshot))
      }
    }

  private def getSnapshotsToSave(
      blockNumber: BigInt,
      snapshots: Seq[StoredNodeSnapshot]
  ): Seq[(NodeHash, Array[Byte])] =
    if (snapshots.nonEmpty) {
      // If not empty, snapshots will be stored indexed by block number and index
      val snapshotCountKey = getSnapshotsCountKey(blockNumber)
      val getSnapshotKeyFn = getSnapshotKey(blockNumber)(_)
      val blockNumberSnapshotsCount: BigInt =
        nodeStorage.get(snapshotCountKey).map(snapshotsCountFromBytes).getOrElse(0)
      val snapshotsToSave = snapshots.zipWithIndex.map { case (snapshot, index) =>
        getSnapshotKeyFn(blockNumberSnapshotsCount + index) -> snapshotToBytes(snapshot)
      }
      // Save snapshots and latest snapshot index
      (snapshotCountKey -> snapshotsCountToBytes(blockNumberSnapshotsCount + snapshotsToSave.size)) +: snapshotsToSave
    } else Nil

  private def getFromChangesOrStorage(
      nodeKey: NodeHash,
      storedNodes: Changes
  ): Option[(StoredNode, StoredNodeSnapshot)] =
    storedNodes
      .get(nodeKey)
      .orElse(nodeStorage.get(nodeKey).map(storedNodeFromBytes).map(sn => sn -> StoredNodeSnapshot(nodeKey, Some(sn))))
}

object ReferenceCountNodeStorage extends PruneSupport with Logger {

  val nodeKeyLength = 32

  def drRowKey(bn: BigInt): ByteString =
    ByteString("dr".getBytes()) ++ ByteString(bn.toByteArray)

  def getDeathRow(key: ByteString, nodeStorage: NodesStorage): ByteString =
    ByteString(nodeStorage.get(key).getOrElse(Array[Byte]()))

  type Changes = Map[NodeHash, (StoredNode, StoredNodeSnapshot)]

  /** Fetches snapshots stored in the DB for the given block number and deletes the stored nodes, referred to
    * by these snapshots, that meet criteria for deletion (see `getNodesToBeRemovedInPruning` for details).
    *
    * All snapshots for this block are removed, which means state can no longer be rolled back to this point.
    *
    * @param blockNumber BlockNumber to prune
    * @param nodeStorage NodeStorage
    */
  override def prune(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = {
    log.debug(s"Pruning block $blockNumber")

    withSnapshotCount(blockNumber, nodeStorage) { (snapshotsCountKey, snapshotCount) =>
      val deathRowKey = drRowKey(blockNumber)
      val snapshotKeys: Seq[NodeHash] = snapshotKeysUpTo(blockNumber, snapshotCount)
      val toBeRemoved = getNodesToBeRemovedInPruning(blockNumber, deathRowKey, nodeStorage)
      nodeStorage.updateCond((deathRowKey +: snapshotsCountKey +: snapshotKeys) ++ toBeRemoved, Nil, inMemory)
    }

    log.debug(s"Pruned block $blockNumber")
  }

  /** Looks for the StoredNode snapshots based on block number and saves (or deletes) them
    *
    * @param blockNumber BlockNumber to rollback
    * @param nodeStorage NodeStorage
    */
  override def rollback(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit =
    withSnapshotCount(blockNumber, nodeStorage) { (snapshotsCountKey, snapshotCount) =>
      // Get all the snapshots
      val snapshots = snapshotKeysUpTo(blockNumber, snapshotCount)
        .flatMap(key => nodeStorage.get(key).map(snapshotFromBytes))
      // We need to delete deathrow for rollbacked block
      val deathRowKey = drRowKey(blockNumber)
      // Transform them to db operations
      val (toRemove, toUpsert) = snapshots.foldLeft(Seq.empty[NodeHash], Seq.empty[(NodeHash, NodeEncoded)]) {
        // Undo Actions
        case ((r, u), StoredNodeSnapshot(nodeHash, Some(sn))) => (r, (nodeHash -> storedNodeToBytes(sn)) +: u)
        case ((r, u), StoredNodeSnapshot(nodeHash, None))     => (nodeHash +: r, u)
      }
      // also remove snapshot as we have done a rollback
      nodeStorage.updateCond(toRemove :+ snapshotsCountKey :+ deathRowKey, toUpsert, inMemory)
    }

  private def withSnapshotCount(blockNumber: BigInt, nodeStorage: NodesStorage)(
      f: (ByteString, BigInt) => Unit
  ): Unit = {
    val snapshotsCountKey = getSnapshotsCountKey(blockNumber)
    // Look for snapshot count for given block number
    val maybeSnapshotCount = nodeStorage.get(snapshotsCountKey).map(snapshotsCountFromBytes)
    maybeSnapshotCount match {
      case Some(snapshotCount) => f(snapshotsCountKey, snapshotCount)
      case None                => ()
    }
  }

  private def snapshotKeysUpTo(blockNumber: BigInt, snapshotCount: BigInt): Seq[ByteString] = {
    val getSnapshotKeyFn = getSnapshotKey(blockNumber)(_)
    (BigInt(0) until snapshotCount).map(snapshotIndex => getSnapshotKeyFn(snapshotIndex))
  }

  /** Within death row of this block, it looks for Nodes that are not longer being used in order to remove them
    * from DB. To do so, it checks if nodes marked in death row have still reference count equal to 0 and are not used by future
    * blocks.
    * @param blockNumber
    * @param deadRowKey
    * @param nodeStorage
    * @return
    */
  private def getNodesToBeRemovedInPruning(
      blockNumber: BigInt,
      deadRowKey: ByteString,
      nodeStorage: NodesStorage
  ): Seq[NodeHash] = {
    var nodesToRemove = List.empty[NodeHash]
    val deathRow = getDeathRow(deadRowKey, nodeStorage).grouped(nodeKeyLength)

    deathRow.foreach { key =>
      for {
        node <- nodeStorage.get(key).map(storedNodeFromBytes)
        if node.references == 0 && node.lastUsedByBlock <= blockNumber
      } yield nodesToRemove = key :: nodesToRemove
    }

    nodesToRemove
  }

  /** Wrapper of MptNode in order to store number of references it has.
    *
    * @param nodeEncoded Encoded Mpt Node to be used in MerklePatriciaTrie
    * @param references  Number of references the node has. Each time it's updated references are increased and everytime it's deleted, decreased
    * @param lastUsedByBlock Block Number where this node was last used
    */
  case class StoredNode(nodeEncoded: ByteString, references: Int, lastUsedByBlock: BigInt) {
    def incrementReferences(amount: Int, blockNumber: BigInt): StoredNode =
      copy(references = references + amount, lastUsedByBlock = blockNumber)

    def decrementReferences(amount: Int, blockNumber: BigInt): StoredNode =
      copy(references = references - amount, lastUsedByBlock = blockNumber)
  }

  object StoredNode {
    def withoutReferences(nodeEncoded: Array[Byte]): StoredNode = new StoredNode(ByteString(nodeEncoded), 0, 0)
  }

  /** Key to be used to store BlockNumber -> Snapshots Count
    *
    * @param blockNumber Block Number Tag
    * @return Key
    */
  private def getSnapshotsCountKey(blockNumber: BigInt): ByteString = ByteString(
    "sck".getBytes ++ blockNumber.toByteArray
  )

  /** Returns a snapshot key given a block number and a snapshot index
    * @param blockNumber Block Number Ta
    * @param index Snapshot Index
    * @return
    */
  private def getSnapshotKey(blockNumber: BigInt)(index: BigInt): ByteString = ByteString(
    ("sk".getBytes ++ blockNumber.toByteArray) ++ index.toByteArray
  )

  /** Used to store a node snapshot in the db. This will be used to rollback a transaction.
    * @param nodeKey Node's key
    * @param storedNode Stored node that can be rolledback. If None, it means that node wasn't previously in the DB
    */
  case class StoredNodeSnapshot(nodeKey: NodeHash, storedNode: Option[StoredNode])

}
