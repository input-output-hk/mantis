package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{PruningNodesKeyValueStorage, PruneResult, RangePrune}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.rlp.RLPImplicitConversions.{toRlpList, _}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

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
  *
  */
class ReferenceCountNodeStorage(nodeStorage: NodeStorage, pruningOffset: BigInt, blockNumber: Option[BigInt] = None)
  extends PruningNodesKeyValueStorage
  with RangePrune {

  import ReferenceCountNodeStorage._

  override def get(key: ByteString): Option[NodeEncoded] = nodeStorage.get(key).map(_.toStoredNode).map(_.nodeEncoded.toArray)

  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {

    require(blockNumber.isDefined)

    val bn = blockNumber.get
    val upsertChanges = toUpsert.foldLeft(Map.empty[NodeHash, StoredNode]) { (storedNodes, toUpsertItem) =>
      val (nodeKey, nodeEncoded) = toUpsertItem
      val storedNode: StoredNode = storedNodes.get(nodeKey)
        .orElse(nodeStorage.get(nodeKey).map(_.toStoredNode))
        .getOrElse(StoredNode(ByteString(nodeEncoded), 0))
        .incrementReferences(1)

      storedNodes + (nodeKey -> storedNode)
    }

    val toPruneInThisBlockKey = pruneKey(bn)
    val pruneCandidates = nodeStorage.get(toPruneInThisBlockKey).map(_.toPruneCandidates).getOrElse(PruneCandidates(Nil))

    val changes = toRemove.foldLeft(upsertChanges, pruneCandidates) { (acc, nodeKey) =>
      val (storedNodes, toDeleteInBlock) = acc
      val storedNode: Option[StoredNode] = storedNodes.get(nodeKey)
        .orElse(nodeStorage.get(nodeKey).map(_.toStoredNode))
        .map(_.decrementReferences(1))

      if (storedNode.isDefined) {
        val pruneCanditatesUpdated =
          if (storedNode.get.references == 0) toDeleteInBlock.copy(nodeKeys = nodeKey +: toDeleteInBlock.nodeKeys)
          else toDeleteInBlock
        (storedNodes + (nodeKey -> storedNode.get), pruneCanditatesUpdated)
      }
      else acc
    }

    val toUpsertUpdated = changes._1.map {
      case (nodeKey: NodeHash, storedNode: StoredNode) => nodeKey -> storedNode.toBytes
    }.toSeq

    val toMarkAsDeleted =
      if (changes._2.nodeKeys.nonEmpty) Seq(toPruneInThisBlockKey -> changes._2.toBytes)
      else Nil


    nodeStorage.update(Nil, toUpsertUpdated ++ toMarkAsDeleted)

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
  private def prune(blockNumber: BigInt, nodeStorage: NodeStorage): Int = {

    val key = pruneKey(blockNumber)

    nodeStorage.get(key)
      .map(_.toPruneCandidates)
      .map { pruneCandidates: PruneCandidates =>
        // Get Node from storage and filter ones which have references = 0 now (maybe they were added again after blockNumber)
        val pruneCandidateNodes = pruneCandidates.nodeKeys.map(nodeStorage.get)
        val pruneCandidateNodesWithKeys = pruneCandidateNodes.zip(pruneCandidates.nodeKeys)
        val nodesToDelete = pruneCandidateNodesWithKeys.filter(n => n._1.isDefined && n._1.get.toStoredNode.references == 0)
        nodesToDelete.map(_._2)
      }.map(nodeKeysToDelete => {
        nodeStorage.update(key +: nodeKeysToDelete, Nil)
        nodeKeysToDelete.size
      }).getOrElse(0)
  }

  /**
    * Model to be used to store, by block number, which block keys are no longer needed (and can potentially be deleted)
    */
  case class PruneCandidates(nodeKeys: Seq[ByteString])

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

  private implicit val storedNodeEncDec = new RLPDecoder[StoredNode] with RLPEncoder[StoredNode] {
    override def decode(rlp: RLPEncodeable): StoredNode = rlp match {
      case RLPList(nodeEncoded, references) => StoredNode(nodeEncoded, references)
      case _ => throw new RuntimeException("Error when decoding stored node")
    }

    override def encode(obj: StoredNode): RLPEncodeable = rlpEncode(RLPList(obj.nodeEncoded, obj.references))
  }

  private implicit val pruneCandidatesEncDec = new RLPEncoder[PruneCandidates] with RLPDecoder[PruneCandidates] {
    override def encode(obj: PruneCandidates): RLPEncodeable = rlpEncode(toRlpList(obj.nodeKeys))

    override def decode(rlp: RLPEncodeable): PruneCandidates = rlp match {
      case RLPList(candidates@_*) => PruneCandidates(candidates.map(b => b: ByteString))
      case _ => throw new RuntimeException("Error when decoding pruning candidate")
    }
  }

  private implicit class StoredToBytes(val storedNode: StoredNode) extends AnyVal {
    def toBytes: Array[Byte] = storedNodeEncDec.encode(storedNode)
  }

  private implicit class PruneCandidatesToBytes(val pruneCanidates: PruneCandidates) extends AnyVal {
    def toBytes: Array[Byte] = pruneCandidatesEncDec.encode(pruneCanidates)
  }

  private implicit class BytesToStoredNode(val array: Array[Byte]) extends AnyVal {
    def toStoredNode: StoredNode = decode(array)(storedNodeEncDec)

    def toPruneCandidates: PruneCandidates = decode(array)(pruneCandidatesEncDec)
  }

  /**
    * Key to be used to store PruneCandidates
    *
    * @param blockNumber Block Number Tag
    * @return Key
    */
  private def pruneKey(blockNumber: BigInt): ByteString = ByteString('d'.toByte +: blockNumber.toByteArray)

}
