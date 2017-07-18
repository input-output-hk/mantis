package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.rlp.RLPImplicitConversions.{toRlpList, _}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  * Key: hash of the RLP encoded node
  * Value: the RLP encoded node
  */
class ReferenceCountNodeStorage(nodeStorage: NodeStorage, blockNumber: Option[BigInt] = None) extends NodesKeyValueStorage {

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
      case (nodeKey: NodeHash, storedNode: StoredNode) =>
        Seq(nodeKey -> storedNode.toBytes)
    }.toSeq.flatten

    val toMarkAsDeleted =
      if (changes._2.nodeKeys.nonEmpty) Seq(toPruneInThisBlockKey -> changes._2.toBytes)
      else Nil


    nodeStorage.update(Nil, toUpsertUpdated ++ toMarkAsDeleted)

    this
  }

  def prune(): Int = {
    require(blockNumber.isDefined)

    val key = pruneKey(blockNumber.get)

    nodeStorage.get(key)
      .map(_.toPruneCandidates)
      .map { pruneCandidates: PruneCandidates =>
        // Get Node from storage and filter ones which have references = 0 now (maybe they were added again after blockNumber)
        pruneCandidates.nodeKeys.map(nodeStorage.get).zip(pruneCandidates.nodeKeys)
          .filter(n => n._1.isDefined && n._1.get.toStoredNode.references == 0)
          .map(_._2)
      }.map(nodesToDelete => {
      nodeStorage.update(key +: nodesToDelete, Nil)
      nodesToDelete.size
    }).getOrElse(0)
  }
}

object ReferenceCountNodeStorage {

  case class PruneCandidates(nodeKeys: Seq[ByteString])

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

  private def pruneKey(blockNumber: BigInt): ByteString = ByteString('d'.toByte +: blockNumber.toByteArray)

}
